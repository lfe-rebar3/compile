%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module(lfe_compiler).

-include("const.hrl").

-export([build_all/3,
         build_deps/2,
         build_projs/2,
         build_apps/2,
         build_app/2,
         compile/2,
         lfe_compile/3]).

%% ===================================================================
%% Public API
%% ===================================================================

build_all(State, DepsPaths, AppInfo) ->
    State1 = build_deps(State, DepsPaths),
    State2 = build_projs(State1, DepsPaths),
    compile(State2, AppInfo),
    State2.

build_deps(State, DepsPaths) ->
    rebar_api:debug("Building dependencies:", []),
    code:add_pathsa(DepsPaths),
    rebar_api:debug("Added code paths: ~p", [DepsPaths]),
    Deps = rebar_state:deps_to_build(State),
    EmptyState = rebar_state:new(),
    DepsState = rebar_state:all_deps(EmptyState, rebar_state:all_deps(State)),
    build_apps(DepsState, Deps),
    State.

build_projs(State, DepsPaths) ->
    rebar_api:debug("Building projects:", []),
    DepsPaths = rebar_state:code_paths(State, all_deps),
    ProjectApps = rebar_state:project_apps(State),
    ProjectApps1 = build_apps(State, ProjectApps),
    State1 = rebar_state:project_apps(State, ProjectApps1),
    ProjAppsPaths = [filename:join(rebar_app_info:out_dir(X), "ebin") || X <- ProjectApps1],
    State2 = rebar_state:code_paths(State1, all_deps, DepsPaths ++ ProjAppsPaths),
    State2.

build_apps(State, Apps) ->
    rebar_api:debug("Building apps ...", []),
    [build_app(State, AppInfo) || AppInfo <- Apps].

build_app(State, AppInfo) ->
    rebar_api:debug("\tBuilding app ~p ...", [rebar_app_info:name(AppInfo)]),
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    copy_app_dirs(State, AppDir, OutDir),

    S = rebar_app_info:state_or_new(State, AppInfo),
    S1 = rebar_state:all_deps(S, rebar_state:all_deps(State)),
    compile(S1, AppInfo).

compile(State, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = filename:join(AppDir, "ebin"),
    rebar_api:debug("\t\tCalculated outdir: ~p", [OutDir]), %% XXX DEBUG
    lfe_compile(State, AppDir, OutDir),
    AppInfo.

-spec lfe_compile(rebar_state:t(), file:name(), file:name()) -> 'ok'.
lfe_compile(State, Dir, OutDir) ->
    dotlfe_compile(State, Dir, OutDir).

%% ===================================================================
%% Internal functions
%% ===================================================================

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
dotlfe_compile(State, Dir, OutDir) ->
    rebar_api:debug("\t\tStarting dotlfe_compile/3 ...", []), %% XXX DEBUG
    ErlOpts = rebar_utils:erl_opts(State),
    LfeFirstFiles = check_files(rebar_state:get(State, lfe_first_files, [])),
    dotlfe_compile(State, Dir, OutDir, [], ErlOpts, LfeFirstFiles).

dotlfe_compile(State, Dir, OutDir, MoreSources, ErlOpts, LfeFirstFiles) ->
    rebar_api:debug("\t\tStarting dotlfe_compile/6 ...", []), %% XXX DEBUG
    rebar_api:debug("\t\terl_opts ~p", [ErlOpts]),
    %% Support the src_dirs option allowing multiple directories to
    %% contain erlang source. This might be used, for example, should
    %% eunit tests be separated from the core application source.
    SrcDirs = [filename:join(Dir, X) || X <- rebar_dir:all_src_dirs(State, ["src"], [])],
    AllLfeFiles = gather_src(SrcDirs, []) ++ MoreSources,

    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),

    OutDir1 = proplists:get_value(outdir, ErlOpts, OutDir),
    rebar_api:debug("\t\tFiles to compile first: ~p", [LfeFirstFiles]),
    rebar_base_compiler:run(
      State, LfeFirstFiles, AllLfeFiles,
      fun(S, C) ->
          internal_lfe_compile(C, Dir, S, OutDir1, ErlOpts)
      end),
    ok.

%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files([file:filename()]) -> [file:filename()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> rebar_utils:abort("\t\tFile ~p is missing, aborting\n", [File]);
        true -> File
    end.

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(
      Rest, Srcs ++ rebar_utils:find_files(Dir, ?RE_PREFIX".*\\.lfe\$")).

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".lfe")).

-spec internal_lfe_compile(rebar_config:config(), file:filename(), file:filename(),
    file:filename(), list()) -> ok | {ok, any()} | {error, any(), any()}.
internal_lfe_compile(Config, Dir, Module, OutDir, ErlOpts) ->
    Target = target_base(OutDir, Module) ++ ".beam",
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...", [Module, Target]),
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
