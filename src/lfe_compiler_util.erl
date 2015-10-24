-module(lfe_compiler_util).

-export([out_dir/0, out_dir/1,
         include_dir/0, include_dir/1,
         get_first_files/2,
         get_files/2,
         get_src_dirs/2,
         target_file/2,
         target_base/2]).

out_dir() ->
    "ebin".

out_dir(AppDir) ->
    filename:join(AppDir, "ebin").

include_dir() ->
    "include".

include_dir(AppDir) ->
    filename:join(AppDir, "include").

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
