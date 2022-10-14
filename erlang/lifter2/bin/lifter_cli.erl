-module(lifter_cli).

-include_lib("kernel/include/logger.hrl").

-export([lift/1]).
-export([analyze_files/2]).
-export([sort_deps/1]).
-export([missing_deps/1]).
-export([find_rebar_dependencies/1]).

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}])).
-define(PRINT_JSON(X), io:format("~s\n", [jsone:encode(X, [{indent, 2}, {space, 1}])])).

%===================================================================================================
% @doc Lift the current directory.
% information about the dependencies, and a place to set up overrides.
%===================================================================================================
lift(WorkspaceRoot) ->
  ?LOG_INFO("Searching for Rebar3 Projects in " ++ WorkspaceRoot),
  {ok, Projects} = lifter_rebar3:find_all_rebar_projects(WorkspaceRoot),

  % 1. find all rebar.configs to find all deps
  % 2. flatten all deps
  % 3. build dep lookup table from module/include to dep
  % 4. check that all modules/includes missing from the source analysis are accounted by 3)
  % 5. create buildfiles with a target per file

  [RebarConfig] = [binary:list_to_bin(P) || P <- filelib:wildcard(WorkspaceRoot ++ "/rebar.config")],

  AllFiles = [binary:list_to_bin(P) || P <- filelib:wildcard(WorkspaceRoot ++ "/**/*.{erl,hrl}"),
                                       string:find(P, "warp-outputs") == nomatch],
  ?LOG_INFO("Lifting workspace with ~p files", [length(AllFiles)]),

  {ok, Analysis0} = erl_analyzer:analyze(AllFiles),
  Analysis = maps:to_list(Analysis0),

  Missing = #{
              modules => uniq([ Mod || {_, #{ missing_modules := Mod }} <- Analysis ]),
              headers => uniq([ Inc || {_, #{ missing_includes := Inc }} <- Analysis ])
            },

  case Missing of
    #{ modules := [], headers := [] } ->
      {ok, Sort} = lifter_depgraph:sort_files(AllFiles),
      {ok, Results} = cerl_analyzer:analyze(Sort),

      {Oks, Errs} = lists:partition(fun
                                    ({_, #{error := _ }}) -> false;
                                    ({_, _}) -> true
                                   end, maps:to_list(Results)),

      StringyErrs = lists:map(
                      fun ({_, #{ error := Err, path := Path }}) ->
                          {Path, #{ path => Path, error =>
                                    binary:list_to_bin(lists:flatten(io_lib:format("~p", [Err]))) }}
                      end,
                      Errs),

      ?PRINT_JSON(#{
                     oks => maps:from_list(Oks),
                     errs => maps:from_list(StringyErrs)
                    });

    _ ->
      ?PRINT_JSON(Missing)
  end.

%===================================================================================================
% @doc Creates the 3rdparty/rebar.manifest file that includes all the
% information about the dependencies, and a place to set up overrides.
%===================================================================================================

find_rebar_dependencies(WorkspaceRoot) ->
  ?LOG_INFO("Searching for Rebar3 Projects in " ++ WorkspaceRoot),
  {ok, Projects} = lifter_rebar3:find_all_rebar_projects(binary:list_to_bin(WorkspaceRoot)),

  ?LOG_INFO("Flattening transitive dependencies..."),

  {ok, ExternalDeps} = lifter_rebar3:download_and_flatten_dependencies(Projects),

  ?PRINT_JSON(ExternalDeps).

%===================================================================================================
% @doc Computes the missing dependencies for a list of Erlang source and header
% files.
%===================================================================================================

missing_deps(Files) ->
  {ok, Analysis0} = erl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),
  Analysis = maps:to_list(Analysis0),

  Missing = #{
              modules => uniq([ Mod || {_, #{ missing_modules := Mod }} <- Analysis ]),
              headers => uniq([ Inc || {_, #{ missing_includes := Inc }} <- Analysis ])
            },

  io:format("~s\n", [?JSON(Missing)]),
  ok.

%===================================================================================================
% @doc Sort all input files that are Erlang sources by the order in which they
% should be processed/analyzed/compiled.
%
% NOTE: this is a topological sort over a dependency graph where edges go from
% a module to its dependencies.
%===================================================================================================

sort_deps(Files) ->
  case lifter_depgraph:sort_files([ binary:list_to_bin(Path) || Path <- Files ]) of
    {ok, Sort} -> 
     [ io:format("~s\n", [ S ]) || S <- Sort ];
    {error, {found_cycles, Cycles}} ->
      io:format("~s\n", [?JSON(#{ dependency_cycles => Cycles })])
  end.

%===================================================================================================
% @doc Analyze all the Core Erlang trees of all the Erlang sources in the input
% files, and output a list of analyses that includes:
%     * external calls to other modules
%     * exports from this module
%     * type exports from this module
%     * files included as headers
%     * other attributes
%
% These can be used to build a dependency graph.
%===================================================================================================

analyze_files(_WorkspaceRoot, Files) ->
  {ok, Results} = cerl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),

  {Oks, Errs} = lists:partition(fun
                                ({_, #{error := _ }}) -> false;
                                ({_, _}) -> true
                               end, maps:to_list(Results)),

  StringyErrs = lists:map(
                  fun ({_, #{ error := Err, path := Path }}) ->
                      {Path, #{ path => Path, error =>
                                binary:list_to_bin(lists:flatten(io_lib:format("~p", [Err]))) }}
                  end,
                  Errs),

  Json = ?JSON(#{
                 oks => maps:from_list(Oks),
                 errs => maps:from_list(StringyErrs)
                }),

  io:format("~s\n", [ Json ]),
  ok.

%===================================================================================================
% Utilities
%===================================================================================================

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> lists:sort(sets:to_list(sets:from_list(lists:flatten(Xs), [{version, 2}]))).
