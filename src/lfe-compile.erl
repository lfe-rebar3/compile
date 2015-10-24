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

-define(NAMESPACE, lfe).
-define(DESC, "The LFE rebar3 compiler plugin").
-define(DEPS, [{default, app_discovery}]).


%% ===================================================================
%% Public API
%% ===================================================================
init(State) ->
    rebar_api:debug("Initializing {lfe, compile} ...", []),
    Provider = providers:create([
            {name, compile},
            {module, ?MODULE},
            {namespace, ?NAMESPACE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 lfe compile"},
            {short_desc, ?DESC},
            {desc, info(?DESC)},
            {opts, []}
    ]),
    State1 = rebar_state:add_provider(State, Provider),
    rebar_api:debug("Initialized {lfe, compile} ...", []),
    {ok, State1}.

do(State) ->
    rebar_api:debug("Starting do/1 for {lfe, compile} ...", []),
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         AppDir = rebar_app_info:dir(AppInfo),
         AppSrcFile = rebar_app_info:app_file_src(AppInfo),
         AppFile = rebar_app_utils:app_src_to_app(AppDir, AppSrcFile),
         rebar_api:debug("AppSrcFile: ~p", [AppSrcFile]),
         rebar_api:debug("AppFile: ~p", [AppFile]),
         OtherSrcDirs = rebar_dir:src_dirs(Opts),
         rebar_api:debug("OtherSrcDirs: ~p", [OtherSrcDirs]),
         SourceDirs = lfe_compiler_util:get_src_dirs(AppDir, ["src"] ++ OtherSrcDirs),
         %%OutDir = lfe_compiler_util:out_dir(AppDir),
         OutDir = filename:join(rebar_app_info:out_dir(AppInfo), "ebin"),
         FirstFiles = lfe_compiler_util:get_first_files(Opts, AppDir),
         Files = lfe_compiler_util:get_files(FirstFiles, SourceDirs),
         rebar_api:debug("AppInfoDir: ~p", [AppDir]),
         rebar_api:debug("SourceDirs: ~p", [SourceDirs]),
         rebar_api:debug("OutDir: ~p", [OutDir]),
         rebar_api:debug("FirstFiles: ~p", [FirstFiles]),
         rebar_api:debug("Files: ~p", [Files]),
         %%code:add_patha(AppDir),
         CompileFun = fun(Source, Opts1) ->
                              lfe_compiler:compile(Opts1, Source, AppDir, OutDir)
                      end,
         rebar_base_compiler:run(Opts, [], Files, CompileFun)
     end || AppInfo <- Apps],
    {ok, State}.

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
