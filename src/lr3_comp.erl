%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module(lr3_comp).

-include("lr3_const.hrl").

-export([compile/4, compile/5,
         compile_normal_apps/1]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(State, Source, AppDir, OutDir) ->
    rebar_api:debug("\t\tEntered compile/3 ...", []),
    ErlOpts = rebar_opts:erl_opts(State),
    compile(State, Source, AppDir, OutDir, ErlOpts).

compile(_State, Source, _AppDir, OutDir, ErlOpts) ->
    Target = lr3_comp_util:target_file(OutDir, Source),
    rebar_api:debug("\t\tEntered compile/4 ...", []),
    rebar_api:debug("\t\tSource: ~p~n\t\tOutDir: ~p", [Source, OutDir]),
    rebar_api:debug("\t\tErlOpts: ~p", [ErlOpts]),
    rebar_api:debug("\t\tTarget: ~p", [Target]),
    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...", [Source, Target]),
    Opts = [{outdir, OutDir}] ++ ErlOpts ++
       [{i, lr3_comp_util:include_dir()}, return, verbose],
    rebar_api:debug("\t\tOpts: ~p", [Opts]),
    CompileResults = lfe_comp:file(Source, Opts),
    rebar_api:debug("Compile results: ~p", [CompileResults]),
    case lfe_comp:file(Source, Opts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Opts)
    end.

compile_normal_apps(State) ->
    rebar_api:debug("Compiling normal LFE apps ...", []),
    Apps = lr3_comp_util:get_apps(State),
    [begin
         lr3_comp_util:copy_app_src(AppInfo),
         Opts = rebar_app_info:opts(AppInfo),
         AppDir = rebar_app_info:dir(AppInfo),
         OtherSrcDirs = rebar_dir:src_dirs(Opts),
         rebar_api:debug("OtherSrcDirs: ~p", [OtherSrcDirs]),
         SourceDirs = lr3_comp_util:get_src_dirs(AppDir, ["src"] ++ OtherSrcDirs),
         %%OutDir = 'lfe-compiler-util':out_dir(AppDir),
         OutDir = filename:join(rebar_app_info:out_dir(AppInfo), "ebin"),
         FirstFiles = lr3_comp_util:get_first_files(Opts, AppDir),
         Files = lr3_comp_util:get_files(FirstFiles, SourceDirs),
         rebar_api:debug("AppInfoDir: ~p", [AppDir]),
         rebar_api:debug("SourceDirs: ~p", [SourceDirs]),
         rebar_api:debug("OutDir: ~p", [OutDir]),
         rebar_api:debug("FirstFiles: ~p", [FirstFiles]),
         rebar_api:debug("Files: ~p", [Files]),
         CompileFun = fun(Source, Opts1) ->
                        rebar_api:console("  ~~~~> \tCompiling ~s ...",
                                          [lr3_comp_util:relative(Source)]),
                        compile(Opts1, Source, AppDir, OutDir)
                      end,
         rebar_base_compiler:run(Opts, [], Files, CompileFun)
     end || AppInfo <- Apps],
    {ok, State}.


