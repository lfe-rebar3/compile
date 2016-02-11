%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%% Copyright (c) 2016, Eric Bailey <eric@ericb.me>
%%
-module(lr3_comp).

-include("lr3_const.hrl").

-export([compile/3,
         compile_dir/4,
         compile_normal_apps/1]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Source, Target, Config) ->
    rebar_api:console(" ~~~~> \tCompiling ~s ...",
                      [lr3_comp_util:relative(Source)]),
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...",
                    [Source, Target]),
    rebar_api:debug("\t\tConfig: ~p", [Config]),
    CompileResults = lfe_comp:file(Source, Config),
    rebar_api:debug("\tCompile results: ~p", [CompileResults]),
    case CompileResults of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config)
    end.

compile_dir(Config, FirstFiles, SourceDir, TargetDir) ->
    ensure_dir(TargetDir),
    SourceExt = ".lfe",
    TargetExt = ".beam",
    rebar_base_compiler:run(Config, FirstFiles, SourceDir, SourceExt, TargetDir,
                            TargetExt, fun compile/3).

compile_normal_apps(State) ->
    rebar_api:debug("\tCompiling normal LFE apps ...", []),
    Apps = lr3_comp_util:get_apps(State),
    [compile_normal_app(AppInfo) || AppInfo <- Apps],
    {ok, State}.

compile_normal_app(AppInfo) ->
    lr3_comp_util:copy_app_src(AppInfo),
    Opts         = rebar_app_info:opts(AppInfo),
    AppDir       = rebar_app_info:dir(AppInfo),
    OtherSrcDirs = rebar_dir:src_dirs(Opts),
    SourceDirs   = lr3_comp_util:get_src_dirs(AppDir, ["src"] ++ OtherSrcDirs),
    OutDir       = lr3_comp_util:relative_out_dir(AppInfo),
    FirstFiles   = lr3_comp_util:get_first_files(Opts, AppDir),
    ErlOpts      = rebar_opts:erl_opts(Opts),
    Config       = config(OutDir, ErlOpts),
    rebar_api:debug("\tOtherSrcDirs: ~p", [OtherSrcDirs]),
    rebar_api:debug("\tAppInfoDir: ~p", [AppDir]),
    rebar_api:debug("\tSourceDirs: ~p", [SourceDirs]),
    rebar_api:debug("\tOutDir: ~p", [OutDir]),
    rebar_api:debug("\tFirstFiles: ~p", [FirstFiles]),
    rebar_api:debug("\tErlOpts: ~p", [ErlOpts]),
    CompileDir = fun(Dir) -> compile_dir(Config, FirstFiles, Dir, OutDir) end,
    lists:foreach(CompileDir, SourceDirs),
    rebar_api:debug("\tFinished compile.", []),
    lr3_comp_util:copy_beam_files(AppInfo, OutDir),
    code:add_patha(lr3_comp_util:out_dir(rebar_app_info:dir(AppInfo))).

-spec config(file:dirname(), list()) -> list().
config(OutDir, ErlOpts) ->
    [{outdir, OutDir}] ++ ErlOpts ++
        [{i, lr3_comp_util:include_dir()}, return, verbose].

-spec ensure_dir(file:dirname()) -> ok.
ensure_dir(OutDir) ->
    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    AbsOutDir = filename:absname(OutDir),
    rebar_api:debug("\t\tAdding ~p to path ...", [AbsOutDir]),
    true = code:add_patha(AbsOutDir),
    ok.
