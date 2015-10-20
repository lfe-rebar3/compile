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
    Dir = ec_cnv:to_list(rebar_app_info:out_dir(AppInfo)),
    compile(rebar_app_info:opts(AppInfo), Dir, filename:join([Dir, "ebin"])).

compile(Opts, Dir, OutDir) ->
    compile(Opts, Dir, OutDir, []).

compile(Opts, Dir, OutDir, More) ->
    ErlOpts = rebar_opts:erl_opts(Opts),
    LfeOpts = check_files(rebar_opts:get(Opts, lfe_first_files, [])),
    dotlfe_compile(Opts, Dir, OutDir, More, ErlOpts, LfeOpts).

%% ===================================================================
%% Internal functions
%% ===================================================================

dotlfe_compile(State, Dir, OutDir) ->
    rebar_api:debug("\t\tStarting dotlfe_compile/3 ...", []),
    ErlOpts = rebar_utils:erl_opts(State),
    LfeFirstFiles = check_files(rebar_state:get(State, lfe_first_files, [])),
    dotlfe_compile(State, Dir, OutDir, [], ErlOpts, LfeFirstFiles).

dotlfe_compile(State, Dir, OutDir, MoreSources, ErlOpts, LfeFirstFiles) ->
    rebar_api:debug("\t\tStarting dotlfe_compile/6 ...", []),
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
    try
      rebar_base_compiler:run(
        State, LfeFirstFiles, AllLfeFiles,
        fun(S, C) ->
            internal_lfe_compile(C, Dir, S, OutDir1, ErlOpts)
        end)
    catch
      _ -> true
    end,
    ok.

gather_src([], Srcs) ->
    rebar_api:debug("\t\tsrc_files: ~p", [Srcs]),
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(
      Rest, Srcs ++ rebar_utils:find_files(Dir, ?RE_PREFIX".*\\.lfe\$")).

%%
%% Ensure all files in a list are present and abort if one is missing
%%
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> rebar_utils:abort("\t\tFile ~p is missing, aborting\n", [File]);
        true -> File
    end.

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".lfe")).

internal_lfe_compile(Config, Dir, Source, OutDir, ErlOpts) ->
    Target = target_base(OutDir, Source) ++ ".beam",
    rebar_api:debug("\t\tCompiling~n\t\t\t~p~n\t\t\tto ~p ...", [Source, Target]),
    ok = filelib:ensure_dir(Target),
    Opts = [{outdir, filename:dirname(Target)}] ++ ErlOpts ++
        [{i, filename:join(Dir, "include")}, return],
    case lfe_comp:file(Source, Opts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Config, Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Config, Source, Es, Ws, Opts)
    end.

