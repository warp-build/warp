-module(warp_lifter).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

main(Args) ->
  ok = setup(),

  {ok, WorkspaceRoot} = file:get_cwd(),

  Result = case Args of
             [] -> show_help();
             ["help"] -> show_help();
             ["create-manifest"] -> create_manifest(WorkspaceRoot);
             ["create-3rdparty-buildfiles"] -> create_3rdparty_buildfiles(WorkspaceRoot);
             ["create-buildfiles"] -> create_buildfiles(WorkspaceRoot)
           end,

  case Result of
    ok -> io:format("OK!\n");
    {error, Reason} -> ?LOG_ERROR("Uh-oh! Something went wrong:\n\n~p\n", [Reason])
  end.

setup() ->
  logger:set_primary_config(#{ level => all }),
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  ok.

show_help() ->
  io:format("~s", [<<"

You can call the erlang lifter with 3 parameters:

* create-manifest
* create-3rdparty-buildfiles
* create-buildfiles

  ">>]),
  ok.

%===============================================================================
% Creates the 3rdparty/rebar.manifest file that includes all the information
% about the dependencies, and a place to set up overrides.
%===============================================================================

create_manifest(WorkspaceRoot) ->
  ?LOG_INFO("Searching for Rebar3 Projects in " ++ WorkspaceRoot),
  {ok, Projects} = wl_rebar3:find_all_rebar_projects(WorkspaceRoot),

  ?LOG_INFO("Flattening transitive dependencies..."),
  {ok, _ThirdpartyDeps} = wl_rebar3:flat_deps(Projects),

  ok.


%===============================================================================
% Reads the 3rdparty/rebar.manifest, and creates all the buildfiles accordingly.
%===============================================================================

create_3rdparty_buildfiles(WorkspaceRoot) ->
  ?LOG_INFO("Creating 3rdparty Build.toml files in " ++ WorkspaceRoot),

  {ok, Overrides} = wl_3rdparty:read_overrides(WorkspaceRoot),
  ?LOG_INFO("With overrides ~p\n", [Overrides]),

  {ok, Projects} = wl_rebar3:find_all_rebar_projects(wl_rebar3:basedir()),

  ?LOG_INFO("Creating 3rdparty build files..."),
  {ok, ThirdpartyBuildfiles} = wl_3rdparty:buildfiles(Projects, Overrides),
  ok = wl_buildfile:write_all(ThirdpartyBuildfiles),

  ok.



%===============================================================================
% Scans source code and scaffolds buildfiles for the entire repository,
% referencing 3rdparty dependencies where appropriate.
%===============================================================================

create_buildfiles(WorkspaceRoot) ->
  ?LOG_INFO("Searching for Rebar3 Projects in " ++ WorkspaceRoot),
  {ok, Projects} = wl_rebar3:find_all_rebar_projects(WorkspaceRoot),

  ?LOG_INFO("Creating module dependency graph..."),
  {ok, Buildfiles} = wl_dep_graph:buildfiles(Projects),

  ok = wl_buildfile:write_all(Buildfiles).
