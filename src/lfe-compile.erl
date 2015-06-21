%% Copyright (c) 2009, Dave Smith <dizzyd@dizzyd.com> &
%%                     Tim Dysinger <tim@dysinger.net>
%% Copyright (c) 2014, 2015 Duncan McGreggor <oubiwann@gmail.com>
%%
-module('lfe-compile').
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([compile/3,
         lfe_compile/2,
         lfe_compile/3]).

-include_lib("stdlib/include/erl_compile.hrl").

-define(PROVIDER, compile).
-define(DESC, "The LFE rebar3 compiler plugin").
-define(DEPS, [{default, compile},
               {default, app_discovery}]).
-define(ERLCINFO_VSN, 2).
-define(ERLCINFO_FILE, "erlcinfo").
-type erlc_info_v() :: {digraph:vertex(), term()} | 'false'.
-type erlc_info_e() :: {digraph:vertex(), digraph:vertex()}.
-type erlc_info() :: {list(erlc_info_v()), list(erlc_info_e()), list(string())}.
-record(erlcinfo,
        {
          vsn = ?ERLCINFO_VSN :: pos_integer(),
          info = {[], [], []} :: erlc_info()
        }).
-define(RE_PREFIX, "^[^._]").

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
    rebar_log:log(debug, "Starting do/1 for {lfe, compile} ...", []), %% XXX DEBUG
    DepsPaths = rebar_state:code_paths(State, all_deps),
    rebar_log:log(debug, "DepsPaths ~p", [DepsPaths]), %% XXX DEBUG
    PluginDepsPaths = rebar_state:code_paths(State, all_plugin_deps),
    rebar_log:log(debug, "PluginDepsPaths ~p", [PluginDepsPaths]), %% XXX DEBUG
    rebar_utils:remove_from_code_path(PluginDepsPaths),
    code:add_pathsa(DepsPaths),

    ProjectApps = rebar_state:project_apps(State),
    Providers = rebar_state:providers(State),
    Deps = rebar_state:deps_to_build(State),
    Cwd = rebar_state:dir(State),

    %% Need to allow global config vars used on deps.
    %% Right now no way to differeniate and just give deps a new state.
    %% But need an account of "all deps" for some hooks to use.
    EmptyState = rebar_state:new(),
    build_apps(rebar_state:all_deps(EmptyState,
                                   rebar_state:all_deps(State)), Providers, Deps),

    {ok, ProjectApps1} = rebar_digraph:compile_order(ProjectApps),

    %% Run top level hooks *before* project apps compiled but *after* deps are
    rebar_log:log(debug, "Preparing to run pre hooks ...", []), %% XXX DEBUG
    rebar_log:log(debug, "Cwd: ~p ", [Cwd]), %% XXX DEBUG
    rebar_log:log(debug, "PROVIDER: ~p ", [?PROVIDER]), %% XXX DEBUG
    DebugProv = lists:map(fun (X) -> lists:flatten(io_lib:format("{~p, ~p, ~p ...}", [element(1, X), element(2, X), element(3, X)])) end, Providers),
    rebar_log:log(debug, "Providers: ~p ", [DebugProv]), %% XXX DEBUG
    rebar_hooks:run_provider_hooks(Cwd, pre, {lfe, ?PROVIDER}, Providers, State),

    ProjectApps2 = build_apps(State, Providers, ProjectApps1),
    rebar_log:log(debug, "ProjectApps2: ~p ", [ProjectApps2]), %% XXX DEBUG 
    State2 = rebar_state:project_apps(State, ProjectApps2),

    ProjAppsPaths = [filename:join(rebar_app_info:out_dir(X), "ebin") || X <- ProjectApps2],
    rebar_log:log(debug, "ProjAppsPaths: ~p ", [ProjAppsPaths]), %% XXX DEBUG 
    State3 = rebar_state:code_paths(State2, all_deps, DepsPaths ++ ProjAppsPaths),

    rebar_log:log(debug, "Preparing to run post hooks ...", []), %% XXX DEBUG
    rebar_hooks:run_provider_hooks(Cwd, post, {lfe, ?PROVIDER}, Providers, State2),
    has_all_artifacts(State3),

    rebar_utils:cleanup_code_path(rebar_state:code_paths(State3, default)),

    {ok, State3}.
    
-spec format_error(any()) -> iolist().
format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

build_apps(State, Providers, Apps) ->
    [build_app(State, Providers, AppInfo) || AppInfo <- Apps].

build_app(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    copy_app_dirs(State, AppDir, OutDir),

    S = rebar_app_info:state_or_new(State, AppInfo),
    S1 = rebar_state:all_deps(S, rebar_state:all_deps(State)),
    compile(S1, Providers, AppInfo).

compile(State, Providers, AppInfo) ->
    rebar_log:log(info, "Compiling ~s", [rebar_app_info:name(AppInfo)]),
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_provider_hooks(AppDir, pre, {lfe, ?PROVIDER},  Providers, State),

    lfe_compile(State, ec_cnv:to_list(rebar_app_info:out_dir(AppInfo))),
    case rebar_otp_app:compile(State, AppInfo) of
        {ok, AppInfo1} ->
            rebar_hooks:run_provider_hooks(AppDir, post, ?PROVIDER, Providers, State),
            has_all_artifacts(State),
            AppInfo1;
        Error ->
            throw(Error)
    end.

-spec lfe_compile(rebar_state:t(), file:name()) -> 'ok'.
lfe_compile(Config, Dir) ->
    lfe_compile(Config, Dir, filename:join([Dir, "ebin"])).

-spec lfe_compile(rebar_state:t(), file:name(), file:name()) -> 'ok'.
lfe_compile(Config, Dir, OutDir) ->
    dotlfe_compile(Config, Dir, OutDir).

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

has_all_artifacts(State) ->
    case rebar_state:has_all_artifacts(State) of
        {false, File} ->
            Reason = {missing_artifact, File},
            throw({error, {?MODULE, Reason}});
        true ->
            true
    end.

copy_app_dirs(State, OldAppDir, AppDir) ->
    case ec_cnv:to_binary(filename:absname(OldAppDir)) =/=
        ec_cnv:to_binary(filename:absname(AppDir)) of
        true ->
            EbinDir = filename:join([OldAppDir, "ebin"]),
            %% copy all files from ebin if it exists
            case filelib:is_dir(EbinDir) of
                true ->
                    OutEbin = filename:join(AppDir, "ebin"),
                    filelib:ensure_dir(filename:join(OutEbin, "dummy.beam")),
                    rebar_file_utils:cp_r(filelib:wildcard(filename:join(EbinDir, "*")), OutEbin);
                false ->
                    ok
            end,
            filelib:ensure_dir(filename:join(AppDir, "dummy")),
            %% link to src_dirs to be adjacent to ebin is needed for R15 use of cover/xref
            SrcDirs = rebar_dir:all_src_dirs(State, ["src"], ["test"]),
            [symlink_or_copy(OldAppDir, AppDir, Dir) || Dir <- ["priv", "include"] ++ SrcDirs];
        false ->
            ok
    end.

symlink_or_copy(OldAppDir, AppDir, Dir) ->
    Source = filename:join(OldAppDir, Dir),
    Target = filename:join(AppDir, Dir),
    rebar_file_utils:symlink_or_copy(Source, Target).


-spec dotlfe_compile(rebar_state:t(), file:filename(), file:filename()) -> ok.
dotlfe_compile(State, Dir, ODir) ->
    rebar_log:log(debug, "Starting dotlfe_compile/3 ...", []), %% XXX DEBUG
    ErlOpts = rebar_utils:erl_opts(State),
    LfeFirstFiles = check_files(rebar_state:get(State, lfe_first_files, [])),
    dotlfe_compile(State, Dir, ODir, [], ErlOpts, LfeFirstFiles).

dotlfe_compile(Config, Dir, OutDir, MoreSources, ErlOpts, LfeFirstFiles) ->
    rebar_log:log(debug, "Starting dotlfe_compile/6 ...", []), %% XXX DEBUG
    rebar_log:log(debug, "erl_opts ~p", [ErlOpts]),
    %% Support the src_dirs option allowing multiple directories to
    %% contain erlang source. This might be used, for example, should
    %% eunit tests be separated from the core application source.
    SrcDirs = [filename:join(Dir, X) || X <- rebar_dir:all_src_dirs(Config, ["src"], [])],
    AllLfeFiles = gather_src(SrcDirs, []) ++ MoreSources,

    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),

    OutDir1 = proplists:get_value(outdir, ErlOpts, OutDir),

    G = init_erlcinfo(proplists:get_all_values(i, ErlOpts), AllLfeFiles, Dir),

    NeededLfeFiles = needed_files(G, ErlOpts, Dir, OutDir1, AllLfeFiles),
    {_, ErlOptsFirst} = erl_first_files(Config, ErlOpts, Dir, []),
    {DepLfes, OtherLfes} = lists:partition(
                             fun(Source) -> digraph:in_degree(G, Source) > 0 end,
                             [File || File <- NeededLfeFiles, not lists:member(File, LfeFirstFiles)]),
    DepLfesOrdered = digraph_utils:topsort(digraph_utils:subgraph(G, DepLfes)),
    FirstLfes = LfeFirstFiles ++ lists:reverse(DepLfesOrdered),
    rebar_log:log(debug, "Files to compile first: ~p", [FirstLfes]),
    rebar_base_compiler:run(
      Config, FirstLfes, OtherLfes,
      fun(S, C) ->
              ErlOpts1 = case lists:member(S, LfeFirstFiles) of
                             true -> ErlOptsFirst;
                             false -> ErlOpts
                         end,
              internal_lfe_compile(C, Dir, S, OutDir1, ErlOpts1)
      end),
    ok.

%% Get files which need to be compiled first, i.e. those specified in erl_first_files
%% and parse_transform options.  Also produce specific erl_opts for these first
%% files, so that yet to be compiled parse transformations are excluded from it.
erl_first_files(Config, ErlOpts, Dir, NeededErlFiles) ->
    ErlFirstFilesConf = rebar_state:get(Config, erl_first_files, []),
    NeededSrcDirs = lists:usort(lists:map(fun filename:dirname/1, NeededErlFiles)),
    %% NOTE: order of files here is important!
    ErlFirstFiles =
        [filename:join(Dir, File) || File <- ErlFirstFilesConf,
                                     lists:member(filename:join(Dir, File), NeededErlFiles)],
    {ParseTransforms, ParseTransformsErls} =
        lists:unzip(lists:flatmap(
                      fun(PT) ->
                              PTerls = [filename:join(D, module_to_erl(PT)) || D <- NeededSrcDirs],
                              [{PT, PTerl} || PTerl <- PTerls, lists:member(PTerl, NeededErlFiles)]
                      end, proplists:get_all_values(parse_transform, ErlOpts))),
    ErlOptsFirst = lists:filter(fun({parse_transform, PT}) ->
                                        not lists:member(PT, ParseTransforms);
                                   (_) ->
                                        true
                               end, ErlOpts),
    {ErlFirstFiles ++ ParseTransformsErls, ErlOptsFirst}.

%% Get subset of SourceFiles which need to be recompiled, respecting
%% dependencies induced by given graph G.
needed_files(G, ErlOpts, Dir, OutDir, SourceFiles) ->
    lists:filter(fun(Source) ->
                         TargetBase = target_base(OutDir, Source),
                         Target = TargetBase ++ ".beam",
                         Opts = [{outdir, filename:dirname(Target)}
                                ,{i, filename:join(Dir, "include")}] ++ ErlOpts,
                         digraph:vertex(G, Source) > {Source, filelib:last_modified(Target)}
                              orelse opts_changed(Opts, TargetBase)
                 end, SourceFiles).

compile_info(Target) ->
    case beam_lib:chunks(Target, [compile_info]) of
        {ok, {_mod, Chunks}} ->
            CompileInfo = proplists:get_value(compile_info, Chunks, []),
            {ok, proplists:get_value(options, CompileInfo, [])};
        {error, beam_lib, Reason} ->
            rebar_log:log(warn, "Couldn't read debug info from ~p for reason: ~p", [Target, Reason]),
            {error, Reason}
    end.

erlcinfo_file(Dir) ->
    filename:join(rebar_dir:local_cache_dir(Dir), ?ERLCINFO_FILE).

%% Get dependency graph of given Erls files and their dependencies (header files,
%% parse transforms, behaviours etc.) located in their directories or given
%% InclDirs. Note that last modification times stored in vertices already respect
%% dependencies induced by given graph G.
init_erlcinfo(InclDirs, Erls, Dir) ->
    G = digraph:new([acyclic]),
    try restore_erlcinfo(G, InclDirs, Dir)
    catch
        _:_ ->
            rebar_log:log(warn, "Failed to restore ~s file. Discarding it.~n", [erlcinfo_file(Dir)]),
            file:delete(erlcinfo_file(Dir))
    end,
    Dirs = source_and_include_dirs(InclDirs, Erls),
    Modified = lists:foldl(update_erlcinfo_fun(G, Dirs), false, Erls),
    if Modified -> store_erlcinfo(G, InclDirs, Dir); not Modified -> ok end,
    G.

source_and_include_dirs(InclDirs, Erls) ->
    SourceDirs = lists:map(fun filename:dirname/1, Erls),
    lists:usort(["include" | InclDirs ++ SourceDirs]).

update_erlcinfo(G, Dirs, Source) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            case filelib:last_modified(Source) of
                0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    modified;
                LastModified when LastUpdated < LastModified ->
                    modify_erlcinfo(G, Source, LastModified, filename:dirname(Source), Dirs);
                _ ->
                    Modified = lists:foldl(
                        update_erlcinfo_fun(G, Dirs),
                        false, digraph:out_neighbours(G, Source)),
                    MaxModified = update_max_modified_deps(G, Source),
                    case Modified orelse MaxModified > LastUpdated of
                        true -> modified;
                        false -> unmodified
                    end
            end;
        false ->
            modify_erlcinfo(G, Source, filelib:last_modified(Source), filename:dirname(Source), Dirs)
    end.

update_erlcinfo_fun(G, Dirs) ->
    fun(Erl, Modified) ->
        case update_erlcinfo(G, Dirs, Erl) of
            modified -> true;
            unmodified -> Modified
        end
    end.

update_max_modified_deps(G, Source) ->
    MaxModified = lists:max(lists:map(
        fun(File) -> {_, MaxModified} = digraph:vertex(G, File), MaxModified end,
        [Source|digraph:out_neighbours(G, Source)])),
    digraph:add_vertex(G, Source, MaxModified),
    MaxModified.

modify_erlcinfo(G, Source, LastModified, Dir, Dirs) ->
    {ok, Fd} = file:open(Source, [read]),
    Incls = parse_attrs(Fd, [], Dir),
    AbsIncls = expand_file_names(Incls, Dirs),
    ok = file:close(Fd),
    digraph:add_vertex(G, Source, LastModified),
    digraph:del_edges(G, digraph:out_edges(G, Source)),
    lists:foreach(
      fun(Incl) ->
              update_erlcinfo(G, Dirs, Incl),
              digraph:add_edge(G, Source, Incl)
      end, AbsIncls),
    modified.

restore_erlcinfo(G, InclDirs, Dir) ->
    case file:read_file(erlcinfo_file(Dir)) of
        {ok, Data} ->
            % Since externally passed InclDirs can influence erlcinfo graph (see
            % modify_erlcinfo), we have to check here that they didn't change.
            #erlcinfo{vsn=?ERLCINFO_VSN, info={Vs, Es, InclDirs}} =
                binary_to_term(Data),
            lists:foreach(
              fun({V, LastUpdated}) ->
                      digraph:add_vertex(G, V, LastUpdated)
              end, Vs),
            lists:foreach(
              fun({_, V1, V2, _}) ->
                      digraph:add_edge(G, V1, V2)
              end, Es);
        {error, _} ->
            ok
    end.

store_erlcinfo(G, InclDirs, Dir) ->
    Vs = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
    Es = lists:map(fun(E) -> digraph:edge(G, E) end, digraph:edges(G)),
    File = erlcinfo_file(Dir),
    ok = filelib:ensure_dir(File),
    Data = term_to_binary(#erlcinfo{info={Vs, Es, InclDirs}}, [{compressed, 2}]),
    file:write_file(File, Data).

%% NOTE: If, for example, one of the entries in Files, refers to
%% gen_server.erl, that entry will be dropped. It is dropped because
%% such an entry usually refers to the beam file, and we don't pass a
%% list of OTP src dirs for finding gen_server.erl's full path. Also,
%% if gen_server.erl was modified, it's not rebar's task to compile a
%% new version of the beam file. Therefore, it's reasonable to drop
%% such entries. Also see process_attr(behaviour, Form, Includes).
-spec expand_file_names([file:filename()],
                        [file:filename()]) -> [file:filename()].
expand_file_names(Files, Dirs) ->
    %% We check if Files exist by itself or within the directories
    %% listed in Dirs.
    %% Return the list of files matched.
    lists:flatmap(
      fun(Incl) ->
              case filelib:is_regular(Incl) of
                  true ->
                      [Incl];
                  false ->
                      lists:flatmap(
                        fun(Dir) ->
                                FullPath = filename:join(Dir, Incl),
                                case filelib:is_regular(FullPath) of
                                    true ->
                                        [FullPath];
                                    false ->
                                        []
                                end
                        end, Dirs)
              end
      end, Files).

%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files([file:filename()]) -> [file:filename()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> rebar_utils:abort("File ~p is missing, aborting\n", [File]);
        true -> File
    end.

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(
      Rest, Srcs ++ rebar_utils:find_files(Dir, ?RE_PREFIX".*\\.lfe\$")).

opts_changed(NewOpts, Target) ->
    case compile_info(Target) of
        {ok, Opts} -> lists:sort(Opts) =/= lists:sort(NewOpts);
        _          -> true
    end.

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".

parse_attrs(Fd, Includes, Dir) ->
    case io:parse_erl_form(Fd, "") of
        {ok, Form, _Line} ->
            case erl_syntax:type(Form) of
                attribute ->
                    NewIncludes = process_attr(Form, Includes, Dir),
                    parse_attrs(Fd, NewIncludes, Dir);
                _ ->
                    parse_attrs(Fd, Includes, Dir)
            end;
        {eof, _} ->
            Includes;
        _Err ->
            parse_attrs(Fd, Includes, Dir)
    end.

process_attr(Form, Includes, Dir) ->
    AttrName = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
    process_attr(AttrName, Form, Includes, Dir).

process_attr(import, Form, Includes, _Dir) ->
    case erl_syntax_lib:analyze_import_attribute(Form) of
        {Mod, _Funs} ->
            [module_to_erl(Mod)|Includes];
        Mod ->
            [module_to_erl(Mod)|Includes]
    end;
process_attr(file, Form, Includes, _Dir) ->
    {File, _} = erl_syntax_lib:analyze_file_attribute(Form),
    [File|Includes];
process_attr(include, Form, Includes, _Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = erl_syntax:string_value(FileNode),
    [File|Includes];
process_attr(include_lib, Form, Includes, Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    RawFile = erl_syntax:string_value(FileNode),
    maybe_expand_include_lib_path(RawFile, Dir) ++ Includes;
process_attr(behaviour, Form, Includes, _Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = module_to_erl(erl_syntax:atom_value(FileNode)),
    [File|Includes];
process_attr(compile, Form, Includes, _Dir) ->
    [Arg] = erl_syntax:attribute_arguments(Form),
    case erl_syntax:concrete(Arg) of
        {parse_transform, Mod} ->
            [module_to_erl(Mod)|Includes];
        {core_transform, Mod} ->
            [module_to_erl(Mod)|Includes];
        L when is_list(L) ->
            lists:foldl(
              fun({parse_transform, Mod}, Acc) ->
                      [module_to_erl(Mod)|Acc];
                 ({core_transform, Mod}, Acc) ->
                      [module_to_erl(Mod)|Acc];
                 (_, Acc) ->
                      Acc
              end, Includes, L);
        _ ->
            Includes
    end;
process_attr(_, _Form, Includes, _Dir) ->
    Includes.

%% Given a path like "stdlib/include/erl_compile.hrl", return
%% "OTP_INSTALL_DIR/lib/erlang/lib/stdlib-x.y.z/include/erl_compile.hrl".
%% Usually a simple [Lib, SubDir, File1] = filename:split(File) should
%% work, but to not crash when an unusual include_lib path is used,
%% utilize more elaborate logic.
maybe_expand_include_lib_path(File, Dir) ->
    File1 = filename:basename(File),
    case filename:split(filename:dirname(File)) of
        [_] ->
            warn_and_find_path(File, Dir);
        [Lib | SubDir] ->
            case code:lib_dir(list_to_atom(Lib), list_to_atom(filename:join(SubDir))) of
                {error, bad_name} ->
                    warn_and_find_path(File, Dir);
                AppDir ->
                    [filename:join(AppDir, File1)]
            end
    end.

%% The use of -include_lib was probably incorrect by the user but lets try to make it work.
%% We search in the outdir and outdir/../include to see if the header exists.
warn_and_find_path(File, Dir) ->
    SrcHeader = filename:join(Dir, File),
    case filelib:is_regular(SrcHeader) of
        true ->
            [SrcHeader];
        false ->
            IncludeDir = filename:join(filename:join(rebar_utils:droplast(filename:split(Dir))), "include"),
            IncludeHeader = filename:join(IncludeDir, File),
            case filelib:is_regular(IncludeHeader) of
                true ->
                    [filename:join(IncludeDir, File)];
                false ->
                    []
            end
    end.

-spec internal_lfe_compile(rebar_config:config(), file:filename(), file:filename(),
    file:filename(), list()) -> ok | {ok, any()} | {error, any(), any()}.
internal_lfe_compile(Config, Dir, Module, OutDir, ErlOpts) ->
    Target = target_base(OutDir, Module) ++ ".beam",
    ok = filelib:ensure_dir(Target),
    Opts = [{outdir, filename:dirname(Target)}] ++ ErlOpts ++
        [{i, filename:join(Dir, "include")}, return],
    case lfe_comp:file(Module, Opts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Config, Module, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Config, Module, Es, Ws, Opts)
    end.
