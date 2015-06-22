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
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
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


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:debug("Starting do/1 for {lfe, compile} ...", []),
    DepsPaths = rebar_state:code_paths(State, all_deps),
    AllApps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
    case rebar_state:get(State, escript_main_app, undefined) of
        undefined ->
            Dir = rebar_state:dir(State),
            case rebar_app_discover:find_app(Dir, all) of
                {true, AppInfo} ->
                    case rebar_app_utils:find(rebar_app_info:name(AppInfo), AllApps) of
                        {ok, AppInfo1} ->
                            %% Use the existing app info instead of newly created one
                            lfe_compiler:build_all(State, DepsPaths, AppInfo1);
                        _ ->
                            lfe_compiler:build_all(State, DepsPaths, AppInfo)
                    end,
                    {ok, State};
                _ ->
                    {error, {?MODULE, no_main_app}}
            end;
        Name ->
            {ok, AppInfo} = rebar_app_utils:find(Name, AllApps),
            {ok, lfe_compiler:build_all(State, DepsPaths, AppInfo)}
    end.


    
-spec format_error(any()) -> iolist().
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
