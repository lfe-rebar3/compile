%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module(lfe_compiler).

-include("const.hrl").

-export([compile/1, compile/3, compile/4]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(AppInfo) ->
    rebar_api:debug("\t\tEntered compile/1 ...", []),
    Dir = ec_cnv:to_list(rebar_app_info:out_dir(AppInfo)),
    compile(rebar_app_info:opts(AppInfo), Dir, filename:join([Dir, "ebin"])).

compile(Opts, Source, OutDir) ->
    rebar_api:debug("\t\tEntered compile/3 ...", []),
    ErlOpts = rebar_opts:erl_opts(Opts),
    compile(Opts, Source, OutDir, ErlOpts).

compile(Opts, Source, OutDir, ErlOpts) ->
    rebar_api:debug("\t\tEntered compile/4 ...", []),
    rebar_api:debug("\t\tSource: ~p~n\t\tOutDir: ~p", [Source, OutDir]),
    rebar_api:debug("\t\tErlOpts: ~p", [ErlOpts]),
    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),
    Target = target_base(OutDir, Source) ++ ".beam",
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...", [Source, Target]),
    Opts = [{outdir, OutDir}] ++ ErlOpts ++
        [{i, include_dir(OutDir)}, return],
    rebar_api:debug("\t\tOpts: ~p", [Opts]),
    case lfe_comp:file(Source, Opts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Opts, Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Opts, Source, Es, Ws, Opts)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".lfe")).

include_dir(OutDir) ->
    filename:join(filename:dirname(OutDir, "include")).

