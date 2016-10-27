%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014-2016 Duncan McGreggor <oubiwann@gmail.com>
%% Copyright (c) 2016, Eric Bailey <eric@ericb.me>
%%
-module('lfe-compile').

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("lr3_const.hrl").

-define(DESC, "The LFE rebar3 compiler plugin").
-define(DEPS, [{default, lock}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar_api:debug("Initializing {lfe, compile} ...", []),
    Provider = providers:create([{name,       ?PROVIDER},
                                 {module,     ?MODULE},
                                 {namespace,  ?NAMESPACE},
                                 {bare,       true},
                                 {deps,       ?DEPS},
                                 {example,    "rebar3 lfe compile"},
                                 {short_desc, ?DESC},
                                 {desc,       info(?DESC)},
                                 {opts,       []}]),
    State1 = rebar_state:add_provider(State, Provider),
    TestRegex = "^[^._].*\\.(erl|lfe)\$",
    State2 = rebar_state:set(State1, eunit_test_regex, TestRegex),
    rebar_api:debug("Initialized {lfe, compile} ...", []),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:debug("Starting do/1 for {lfe, compile} ...", []),
    rebar_api:console(" ~~~~> \tFinding .lfe files ...",[]),
    rebar_api:debug("\tWhere is the rebar_state module? ~p",
                    [code:where_is_file("rebar_state.beam")]),
    lr3_comp:compile_normal_apps(State).

-spec format_error(any()) -> iolist().
format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%% ===================================================================
%% Internal functions
%% ===================================================================

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "No additional configuration options are required to compile~n"
        "LFE (*.lfe) files. The rebar 'erl_opts' setting is reused by~n"
        "LFE. For more information, see the rebar documentation for~n"
        "'erl_opts'.~n",
        [Description]).
