%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%% 
-module('lfe-compile').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [{default, compile},
               {default, app_discovery}]).

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
            {short_desc, "rebar3 compile for LFE"},
            {desc, "The LFE rebar3 compiler plugin"},
            {opts, []}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Cwd = rebar_utils:get_cwd(),
    FirstFiles = check_files(rebar_state:get(State, lfe_first_files, [])),
    Result = rebar_base_compiler:run(State,
                                     FirstFiles,
                                     filename:join(Cwd, "src"),
                                     ".lfe",
                                     filename:join(Cwd, "ebin"),
                                     ".beam",
                                     fun compile_lfe/3),
    {Result, State}.

-spec format_error(any()) ->  iolist().
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
        "'erl_opts'.",
        [Description]).

compile_lfe(Source, _Target, State) ->
    case code:which(lfe_comp) of
        non_existing ->
            ?ERROR("~n"
                   "*** MISSING LFE COMPILER ***~n"
                   "~n", []),
            ?FAIL;
        _ ->
            ErlOpts = rebar_utils:erl_opts(State),
            Opts = [{i, "include"}, {outdir, "ebin"}, return] ++ ErlOpts,
            case lfe_comp:file(Source, Opts) of
                {ok, _Mod, Ws} ->
                    rebar_base_compiler:ok_tuple(State, Source, Ws);
                {error, Es, Ws} ->
                    rebar_base_compiler:error_tuple(State, Source,
                                                    Es, Ws, Opts);
                _ ->
                    ?FAIL
            end
    end.

%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files([file:filename()]) -> [file:filename()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> ?ABORT("File ~p is missing, aborting\n", [File]);
        true -> File
    end.