-module(lr3_comp_util).

-export([copy_app_src/1,
         copy_beam_files/1,
         out_dir/0, out_dir/1,
         include_dir/0, include_dir/1,
         get_apps/1,
         get_first_files/2,
         get_files/2,
         get_src_dirs/2,
         target_file/2,
         target_base/2,
         relative/1]).

copy_app_src(AppInfo) ->
    rebar_api:debug("\t\tEntered copy_app_src/1 ...", []),
    AppDir = rebar_app_info:dir(AppInfo),
    AppSrcFile = rebar_app_info:app_file_src(AppInfo),
    AppFile = rebar_app_utils:app_src_to_app(AppDir, AppSrcFile),
    rebar_api:debug("\t\tCopying ~p to ~p ...", [AppSrcFile, AppFile]),
    copy_file(AppSrcFile, AppFile).

copy_beam_files(AppInfo) ->
    rebar_api:debug("\t\tEntered copy_beam_files/1 ...", []),
    EbinDir = out_dir(rebar_app_info:dir(AppInfo)),
    BeamFiles = filelib:wildcard(filename:join(EbinDir, "*")),
    rebar_api:debug("\t\tCopying ~p to ~p ...", [BeamFiles, EbinDir]),
    [copy_beam_file(BeamFile, EbinDir) || BeamFile <- BeamFiles].

copy_beam_file(BeamFile, EbinDir) ->
    Filename = filename:basename(BeamFile),
    DestFile = filename:join(EbinDir, Filename),
    case BeamFile =:= DestFile of
        true -> rebar_api:debug("\t\tFiles the same; skipping", []);
        false -> copy_file(BeamFile, DestFile)
    end.

copy_file(Src, Dst) ->
    case file:copy(Src, Dst) of
        {ok, BytesCopied} ->
            rebar_api:debug("\t\tCopied ~p bytes.", [BytesCopied]);
        {error, Reason} ->
            rebar_api:error("\t\tFailed to copy ~p: ~p", [Src, Reason])
    end.

out_dir() ->
    "ebin".

out_dir(AppDir) ->
    filename:join(AppDir, "ebin").

relative_out_dir(AppInfo) ->
    filename:join(rebar_app_info:out_dir(AppInfo), "ebin").

include_dir() ->
    "include".

include_dir(AppDir) ->
    filename:join(AppDir, "include").

get_apps(State) ->
    case rebar_state:current_app(State) of
           undefined ->
             rebar_api:debug("\tCurrent app state is undefined ...", []),
             rebar_state:project_apps(State);
           AppInfo ->
             rebar_api:debug("\tConverting current app state to list ...", []),
             [AppInfo]
    end.

get_first_files(Opts, AppDir) ->
    Dirs = rebar_opts:get(Opts, lfe_first_files, []),
    [filename:join(AppDir, Dir) || Dir <- Dirs].

get_files(First, Dirs) ->
    rebar_api:debug("Dirs: ~p", [Dirs]),
    Files = lists:append(
              [rebar_utils:find_files(Dir, ".*\.lfe\$") || Dir <- Dirs]),
    rebar_api:debug("Files: ~p", [Files]),
    NoDuplicates = lists:subtract(lists:usort(Files), First),
    First ++ NoDuplicates.

get_src_dirs(AppDir, Dirs) ->
    rebar_api:debug("Dirs: ~p", [Dirs]),
    lists:usort([filename:join(AppDir, DirName) || DirName <- Dirs]).

target_file(OutDir, Source) ->
    target_base(OutDir, Source) ++ ".beam".

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".lfe")).

relative(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    re:replace(Filename, Cwd, ".", [{return,list}]).
