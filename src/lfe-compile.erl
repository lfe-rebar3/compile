%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module('lfe-compile').
-behaviour(provider).

-include("const.hrl").

-export([init/1,
         do/1,
         format_error/1]).

-define(DESC, "The LFE rebar3 compiler plugin").
-define(DEPS, [{default, compile},
               {default, app_discovery}]).


%% ===================================================================
%% Public API
%% ===================================================================
init(State) ->
    rebar_log:log(debug, "Initializing {lfe, compile} ...", []), %% XXX DEBUG
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
    State1 = rebar_state:add_provider(State, Provider),
    rebar_log:log(debug, "Initialized {lfe, compile} ...", []), %% XXX DEBUG
    {ok, State1}.


do(State) ->
    rebar_api:debug("Starting do/1 for {lfe, compile} ...", []),
    DepsPaths = rebar_state:code_paths(State, all_deps),
    PluginDepsPaths = rebar_state:code_paths(State, all_plugin_deps),
    rebar_utils:remove_from_code_path(PluginDepsPaths),
    code:add_pathsa(DepsPaths),

    ProjectApps = rebar_state:project_apps(State),
    Providers = rebar_state:providers(State),
    Deps = rebar_state:deps_to_build(State),
    Cwd = rebar_state:dir(State),

    build_apps(State, Providers, Deps),
    {ok, ProjectApps1} = rebar_digraph:compile_order(ProjectApps),

    %% Run top level hooks *before* project apps compiled but *after* deps are
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    ProjectApps2 = build_apps(State, Providers, ProjectApps1),
    State2 = rebar_state:project_apps(State, ProjectApps2),

    ProjAppsPaths = [filename:join(rebar_app_info:out_dir(X), "ebin") || X <- ProjectApps2],
    State3 = rebar_state:code_paths(State2, all_deps, DepsPaths ++ ProjAppsPaths),

    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State2),
    case rebar_state:has_all_artifacts(State3) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            true
    end,
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State3, default)
                                 ++ rebar_state:code_paths(State, all_plugin_deps)),

    {ok, State3}.

format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

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

build_apps(State, Providers, Apps) ->
    [build_app(State, Providers, AppInfo) || AppInfo <- Apps].

build_app(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    copy_app_dirs(AppInfo, AppDir, OutDir),
    compile(State, Providers, AppInfo).

compile(State, AppInfo) ->
    compile(State, rebar_state:providers(State), AppInfo).

compile(State, Providers, AppInfo) ->
    ?INFO("Compiling ~s", [rebar_app_info:name(AppInfo)]),
    AppDir = rebar_app_info:dir(AppInfo),
    AppInfo1 = rebar_hooks:run_all_hooks(AppDir, pre, ?PROVIDER,  Providers, AppInfo, State),

    lfe_compiler:compile(AppInfo1),
    case rebar_otp_app:compile(State, AppInfo1) of
        {ok, AppInfo2} ->
            AppInfo3 = rebar_hooks:run_all_hooks(AppDir, post, ?PROVIDER, Providers, AppInfo2, State),
            has_all_artifacts(AppInfo3),
            AppInfo3;
        Error ->
            throw(Error)
    end.

has_all_artifacts(AppInfo1) ->
    case rebar_app_info:has_all_artifacts(AppInfo1) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            true
    end.

copy_app_dirs(AppInfo, OldAppDir, AppDir) ->
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

            %% link or copy mibs if it exists
            case filelib:is_dir(filename:join(OldAppDir, "mibs")) of
                true ->
                    %% If mibs exist it means we must ensure priv exists.
                    %% mibs files are compiled to priv/mibs/
                    filelib:ensure_dir(filename:join([OldAppDir, "priv", "dummy"])),
                    symlink_or_copy(OldAppDir, AppDir, "mibs");
                false ->
                    ok
            end,

            %% link to src_dirs to be adjacent to ebin is needed for R15 use of cover/xref
            SrcDirs = rebar_dir:all_src_dirs(rebar_app_info:opts(AppInfo), ["src"], []),
            [symlink_or_copy(OldAppDir, AppDir, Dir) || Dir <- ["priv", "include"] ++ SrcDirs];
        false ->
            ok
    end.

symlink_or_copy(OldAppDir, AppDir, Dir) ->
    Source = filename:join(OldAppDir, Dir),
    Target = filename:join(AppDir, Dir),
    rebar_file_utils:symlink_or_copy(Source, Target).
