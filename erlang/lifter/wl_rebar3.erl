-module(wl_rebar3).

-include_lib("kernel/include/logger.hrl").

-export([basedir/0]).
-export([config_file/0]).
-export([find_all_rebar_projects/1]).
-export([flat_deps/1]).
-export([libdir/0]).
-export([libdir/1]).
-export([modules/0]).
-export([read_rebar_project/1]).

%===============================================================================
% List of standard rebar3 modules that are only available to plugins.
%===============================================================================

modules() -> [
              rebar_api,
              rebar_app_info,
              rebar_config,
              rebar_dir,
              rebar_file_utils,
              rebar_hex_repos,
              rebar_hooks,
              rebar_log,
              rebar_opts,
              rebar_prv_cover,
              rebar_resource_v2,
              rebar_state,
              rebar_string,
              rebar_utils
             ].

%===============================================================================
% Other constants
%===============================================================================

config_file() -> "rebar.config".

third_party_deps_root() ->
  {ok, Root} = file:get_cwd(),
  filename:join(Root, "3rdparty").

third_party_rebar_config() -> filename:join(third_party_deps_root(), config_file()).

basedir() -> filename:join(third_party_deps_root(), "rebar3").

libdir(X) when is_atom(X) -> libdir(atom_to_list(X));
libdir(X) when is_list(X) -> filename:join([libdir(), X]).

libdir() -> filename:join([basedir(), "default", "lib"]).


%===============================================================================
% Flatten all dependencies in a rebar project, calling rebar repeatedly until
% we have listed all transitive dependencies.
%===============================================================================

flat_deps(Projects) ->
  loop_flatten(0, Projects).

loop_flatten(Iter, Projects) ->
  {ok, CurrentDeps} = wl_dependency:all_deps(Projects),
  ok = install(CurrentDeps),

  {ok, NewProjects} = find_all_rebar_projects(basedir()),
  {ok, FreshDeps} = wl_dependency:all_deps(NewProjects),

  ?LOG_INFO("Verifying all transitive dependencies are specified..."),
  case CurrentDeps == FreshDeps of
    true ->
      ?LOG_INFO("Saving manifest..."),
      Rebar3File = rebar3_file(CurrentDeps),
      ok = write_file(third_party_rebar_config(), Rebar3File),
      {ok, NewProjects};
    false ->
      loop_flatten(Iter + 1, NewProjects)
  end.


install(Deps) ->
  Rebar3File = rebar3_file(Deps),
  ok = write_file(third_party_rebar_config(), Rebar3File),
  ok = in_dir(third_party_deps_root(), fun () -> run_rebar("get-deps") end).

write_file(Path, Contents) ->
  RootPath = filename:split(filename:dirname(Path)),
  mkdirp(RootPath),
  ok = file:write_file(Path, Contents).

mkdirp(Path) -> mkdirp(Path, []).
mkdirp([], _Acc) -> ok;
mkdirp([H|T], Acc0) ->
  Acc1 = filename:join(Acc0, H),
  case file:make_dir(Acc1) of
    ok -> mkdirp(T, Acc1);
    {error, eisdir}-> mkdirp(T, Acc1);
    {error, eexist}-> mkdirp(T, Acc1)
  end.


rebar3_file(Deps) ->
<<"

{base_dir, \"rebar3\"}.

{deps, \n", (wl_dependency:to_rebar_deps_string(Deps))/binary, "\n}.

">>.

run_rebar(Cmd) ->
  ?LOG_INFO("Running `rebar3 " ++ Cmd ++"`...this could take a while!"),
  _ = os:cmd("rebar3 "++Cmd),
  ok.

in_dir(Dir, Fn) ->
  {ok, Cwd} = file:get_cwd(),
  ok = file:set_cwd(Dir),
  Res = Fn(),
  ok = file:set_cwd(Cwd),
  Res.


%===============================================================================
% Find all dependencies in a workspace by folding over all rebar.config files
% and "consulting" them.
%===============================================================================

find_all_rebar_projects(Root) ->
  RebarProjects = filelib:fold_files(Root, "rebar\.config$", true, fun read_rebar_project/2, #{}),
  ErlMkProjects = filelib:fold_files(Root, "erlang\.mk", true, fun read_erlmk_project/2, #{}),
  {ok, maps:merge(RebarProjects, ErlMkProjects)}.

read_rebar_project(RebarConfig) -> read_rebar_project(RebarConfig, #{}).
read_rebar_project(RebarConfig, Acc) ->
  ProjectRoot = filename:dirname(RebarConfig),
  ProjectName = filename:basename(ProjectRoot),

  case ProjectName of
    "3rdparty" -> Acc;
    _ -> do_read_rebar_project(RebarConfig, Acc)
  end.

do_read_rebar_project(RebarConfig, Acc) ->
  ProjectRoot = filename:dirname(RebarConfig),
  ProjectName = filename:basename(ProjectRoot),

  {ok, Project} = file:consult(RebarConfig),

  FinalProject = [
                  {name, ProjectName},
                  {root, ProjectRoot},
                  {srcs, get_sources(ProjectRoot)}
                  | Project
                 ],

  maps:put(ProjectRoot, FinalProject, Acc).

read_erlmk_project(ErlmkConfig, Acc) ->
  ProjectRoot = filename:dirname(ErlmkConfig),
  ProjectName = filename:basename(ProjectRoot),

  case ProjectName of
    "3rdparty" -> Acc;
    _ -> do_read_erlmk_project(ErlmkConfig, Acc)
  end.

do_read_erlmk_project(ErlmkConfig, Acc) ->
  ProjectRoot = filename:dirname(ErlmkConfig),
  ProjectName = filename:basename(ProjectRoot),

  {ok, Project} = wl_erlmk:from_file(filename:join([ProjectRoot, "Makefile"])),

  FinalProject = [
                  {name, ProjectName},
                  {root, ProjectRoot},
                  {srcs, get_sources(ProjectRoot)}
                  | Project
                 ],

  maps:put(ProjectRoot, FinalProject, Acc).

get_sources(ProjectRoot) ->
  SrcGlob = ProjectRoot ++ "/src/**/*.{erl,hrl}",
  TestGlob = ProjectRoot ++ "/test/**/*.{erl,hrl}",
  IncludeGlob = ProjectRoot ++ "/include/**/*.{erl,hrl}",
  PrivGlob = ProjectRoot ++ "/priv/**/*",

  Srcs = #{
           srcs => filelib:wildcard(SrcGlob),
           tests => filelib:wildcard(TestGlob),
           includes => filelib:wildcard(IncludeGlob),
           priv => filelib:wildcard(PrivGlob)
          },

  Srcs.
