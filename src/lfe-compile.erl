%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module('lfe-compile').
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([compile/3,
         lfe_compile/2,
         lfe_compile/3]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, compile).
-define(DESC, "The LFE rebar3 compiler plugin").
-define(DEPS, [{default, compile},
               {default, app_discovery}]).
-define(RE_PREFIX, "^[^._]").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, compile},
            {module, ?MODULE},
            {namespace, lfe},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 lfe compile"},
            {short_desc, ?DESC},
            {desc, info(?DESC)},
            {opts, []}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    DepsPaths = rebar_state:code_paths(State, all_deps),
    PluginDepsPaths = rebar_state:code_paths(State, all_plugin_deps),
    rebar_utils:remove_from_code_path(PluginDepsPaths),
    code:add_pathsa(DepsPaths),

    ProjectApps = rebar_state:project_apps(State),
    Providers = rebar_state:providers(State),
    Deps = rebar_state:deps_to_build(State),
    Cwd = rebar_state:dir(State),

    %% Need to allow global config vars used on deps.
    %% Right now no way to differeniate and just give deps a new state.
    %% But need an account of "all deps" for some hooks to use.
    EmptyState = rebar_state:new(),
    build_apps(rebar_state:all_deps(EmptyState,
                                   rebar_state:all_deps(State)), Providers, Deps),

    {ok, ProjectApps1} = rebar_digraph:compile_order(ProjectApps),

    %% Run top level hooks *before* project apps compiled but *after* deps are
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    ProjectApps2 = build_apps(State, Providers, ProjectApps1),
    State2 = rebar_state:project_apps(State, ProjectApps2),

    ProjAppsPaths = [filename:join(rebar_app_info:out_dir(X), "ebin") || X <- ProjectApps2],
    State3 = rebar_state:code_paths(State2, all_deps, DepsPaths ++ ProjAppsPaths),

    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State2),
    has_all_artifacts(State3),

    rebar_utils:cleanup_code_path(rebar_state:code_paths(State3, default)),

    {ok, State3}.

-spec format_error(any()) -> iolist().
format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

build_apps(State, Providers, Apps) ->
    [build_app(State, Providers, AppInfo) || AppInfo <- Apps].

build_app(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    copy_app_dirs(State, AppDir, OutDir),

    S = rebar_app_info:state_or_new(State, AppInfo),
    S1 = rebar_state:all_deps(S, rebar_state:all_deps(State)),
    compile(S1, Providers, AppInfo).

compile(State, Providers, AppInfo) ->
    rebar_log:log(info, "Compiling ~s", [rebar_app_info:name(AppInfo)]),
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_all_hooks(AppDir, pre, ?PROVIDER,  Providers, State),

    lfe_compile(State, ec_cnv:to_list(rebar_app_info:out_dir(AppInfo))),
    case rebar_otp_app:compile(State, AppInfo) of
        {ok, AppInfo1} ->
            rebar_hooks:run_all_hooks(AppDir, post, ?PROVIDER, Providers, State),
            has_all_artifacts(State),
            AppInfo1;
        Error ->
            throw(Error)
    end.

-spec lfe_compile(rebar_state:t(), file:name()) -> 'ok'.
lfe_compile(Config, Dir) ->
    lfe_compile(Config, Dir, filename:join([Dir, "ebin"])).

-spec lfe_compile(rebar_state:t(), file:name(), file:name()) -> 'ok'.
lfe_compile(Config, Dir, OutDir) ->
    dotlfe_compile(Config, Dir, OutDir).

%% ===================================================================
%% Internal functions
%% ===================================================================
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "No additional configuration options are required to compile~n"
        "LFE (*.lfe) files. The rebar 'erl_opts' setting is reused by~n"
        "LFE. For more information, see the rebar documentation for~n"
        "'erl_opts'.~n",
        [Description]).

has_all_artifacts(State) ->
    case rebar_state:has_all_artifacts(State) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            true
    end.

copy_app_dirs(State, OldAppDir, AppDir) ->
    case ec_cnv:to_binary(filename:absname(OldAppDir)) =/=
        ec_cnv:to_binary(filename:absname(AppDir)) of
        true ->
            EbinDir = filename:join([OldAppDir, "ebin"]),
            %% copy all files from ebin if it exists
            case filelib:is_dir(EbinDir) of
                true ->
                    OutEbin = filename:join(AppDir, "ebin"),
                    filelib:ensure_dir(filename:join(OutEbin, "dummy.beam")),
                    rebar_file_utils:cp_r(filelib:wildcard(filename:join(EbinDir, "*")), OutEbin);
                false ->
                    ok
            end,
            filelib:ensure_dir(filename:join(AppDir, "dummy")),
            %% link to src_dirs to be adjacent to ebin is needed for R15 use of cover/xref
            SrcDirs = rebar_dir:all_src_dirs(State, ["src"], ["test"]),
            [symlink_or_copy(OldAppDir, AppDir, Dir) || Dir <- ["priv", "include"] ++ SrcDirs];
        false ->
            ok
    end.

symlink_or_copy(OldAppDir, AppDir, Dir) ->
    Source = filename:join(OldAppDir, Dir),
    Target = filename:join(AppDir, Dir),
    rebar_file_utils:symlink_or_copy(Source, Target).


-spec dotlfe_compile(rebar_state:t(), file:filename(), file:filename()) -> ok.
dotlfe_compile(State, Dir, ODir) ->
    case code:which(lfe_comp) of
        non_existing ->
            no_lfe();
        _ ->
            ErlOpts = rebar_utils:erl_opts(State),
            dotlfe_compile(State, Dir, ODir, [], ErlOpts).
    end.

doterl_compile(Config, Dir, OutDir, MoreSources, ErlOpts) ->
    rebar_log:log(debug, "erl_opts ~p", [ErlOpts]),
    %% Support the src_dirs option allowing multiple directories to
    %% contain erlang source. This might be used, for example, should
    %% eunit tests be separated from the core application source.
    SrcDirs = [filename:join(Dir, X) || X <- rebar_dir:all_src_dirs(Config, ["src"], [])],
    AllLfeFiles = gather_src(SrcDirs, []) ++ MoreSources,

    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),

    OutDir1 = proplists:get_value(outdir, ErlOpts, OutDir),

    G = init_erlcinfo(proplists:get_all_values(i, ErlOpts), AllLfeFiles, Dir),

    NeededLfeFiles = needed_files(G, ErlOpts, Dir, OutDir1, AllLfeFiles),
    {_, ErlOptsFirst} = erl_first_files(Config, ErlOpts, Dir, NeededErlFiles),
    LfeFirstFiles = check_files(rebar_state:get(State, lfe_first_files, [])),
    {DepLfes, OtherLfes} = lists:partition(
                             fun(Source) -> digraph:in_degree(G, Source) > 0 end,
                             [File || File <- NeededLfeFiles, not lists:member(File, LfeFirstFiles)]),
    DepLfesOrdered = digraph_utils:topsort(digraph_utils:subgraph(G, DepLfes)),
    FirstLfes = LfeFirstFiles ++ lists:reverse(DepLfesOrdered),
    rebar_log:log(debug, ("Files to compile first: ~p", [FirstLfes]),
    rebar_base_compiler:run(
      Config, FirstLfes, OtherLfes,
      fun(S, C) ->
              ErlOpts1 = case lists:member(S, LfeFirstFiles) of
                             true -> LfeOptsFirst;
                             false -> LfeOpts
                         end,
              internal_lfe_compile(C, Dir, S, OutDir1, ErlOpts1)
      end),
    ok.

%% Get subset of SourceFiles which need to be recompiled, respecting
%% dependencies induced by given graph G.
needed_files(G, ErlOpts, Dir, OutDir, SourceFiles) ->
    lists:filter(fun(Source) ->
                         TargetBase = target_base(OutDir, Source),
                         Target = TargetBase ++ ".beam",
                         Opts = [{outdir, filename:dirname(Target)}
                                ,{i, filename:join(Dir, "include")}] ++ ErlOpts,
                         digraph:vertex(G, Source) > {Source, filelib:last_modified(Target)}
                              orelse opts_changed(Opts, TargetBase)
                 end, SourceFiles).
%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files([file:filename()]) -> [file:filename()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> rebar_utils:abort("File ~p is missing, aborting\n", [File]);
        true -> File
    end.

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(
      Rest, Srcs ++ rebar_utils:find_files(Dir, ?RE_PREFIX".*\\.lfe\$")).

-spec internal_lfe_compile(rebar_config:config(), file:filename(), file:filename(),
    file:filename(), list()) -> ok | {ok, any()} | {error, any(), any()}.
internal_lfe_compile(Config, Dir, Module, OutDir, ErlOpts) ->
    Target = target_base(OutDir, Module) ++ ".beam",
    ok = filelib:ensure_dir(Target),
    Opts = [{outdir, filename:dirname(Target)}] ++ ErlOpts ++
        [{i, filename:join(Dir, "include")}, return],
    case lfe_comp:file(Module, Opts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Config, Module, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Config, Module, Es, Ws, Opts)
    end.

no_lfe () ->
    rebar_log:log("~n"
                  "*** MISSING LFE COMPILER ***~n"
                  "~n", []),
    rebar_utils:abort();