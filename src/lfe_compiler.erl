%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module(lfe_compiler).

-include("const.hrl").

-export([compile/4, compile/5]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Opts, Source, AppDir, OutDir) ->
    rebar_api:debug("\t\tEntered compile/3 ...", []),
    ErlOpts = rebar_opts:erl_opts(Opts),
    compile(Opts, Source, AppDir, OutDir, ErlOpts).

compile(Opts, Source, AppDir, OutDir, ErlOpts) ->
    Target = target_base(OutDir, Source) ++ ".beam",
    rebar_api:debug("\t\tEntered compile/4 ...", []),
    rebar_api:debug("\t\tSource: ~p~n\t\tOutDir: ~p", [Source, OutDir]),
    rebar_api:debug("\t\tErlOpts: ~p", [ErlOpts]),
    rebar_api:debug("\t\tTarget: ~p", [Target]),
    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...", [Source, Target]),
    Opts2 = [{outdir, OutDir}] ++ ErlOpts ++
        [{i, include_dir(AppDir)}, return],
    rebar_api:debug("\t\tOpts: ~p", [Opts2]),
    case lfe_comp:file(Source, Opts2) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Opts2, Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Opts2, Source, Es, Ws, Opts)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".lfe")).

include_dir(AppDir) ->
    filename:join(AppDir, "include").

