%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module(lfe_compiler).

-include("const.hrl").

-export([copy_app_src/1, compile/4, compile/5]).

%% ===================================================================
%% Public API
%% ===================================================================

copy_app_src(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppSrcFile = rebar_app_info:app_file_src(AppInfo),
    AppFile = rebar_app_utils:app_src_to_app(AppDir, AppSrcFile),
    rebar_api:debug("AppSrcFile: ~p", [AppSrcFile]),
    rebar_api:debug("AppFile: ~p", [AppFile]),
    file:copy(AppSrcFile, AppFile).

compile(State, Source, AppDir, OutDir) ->
    rebar_api:debug("\t\tEntered compile/3 ...", []),
    ErlOpts = rebar_opts:erl_opts(State),
    compile(State, Source, AppDir, OutDir, ErlOpts).

compile(_State, Source, _AppDir, OutDir, ErlOpts) ->
    Target = lfe_compiler_util:target_file(OutDir, Source),
    rebar_api:debug("\t\tEntered compile/4 ...", []),
    rebar_api:debug("\t\tSource: ~p~n\t\tOutDir: ~p", [Source, OutDir]),
    rebar_api:debug("\t\tErlOpts: ~p", [ErlOpts]),
    rebar_api:debug("\t\tTarget: ~p", [Target]),
    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...", [Source, Target]),
    Opts = [{outdir, OutDir}] ++ ErlOpts ++
       [{i, lfe_compiler_util:include_dir()}, return, verbose],
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




