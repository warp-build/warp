-module(lifter_cli).

-include_lib("kernel/include/logger.hrl").

-export([analyze_files/2]).
-export([sort_deps/1]).
-export([missing_deps/1]).

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}])).

missing_deps(Files) ->
  {ok, Analysis0} = erl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),
  Analysis = maps:to_list(Analysis0),

  Missing = #{
              modules => uniq([ Mod || {_, #{ missing_modules := Mod }} <- Analysis ]),
              headers => uniq([ Inc || {_, #{ missing_includes := Inc }} <- Analysis ])
            },

  io:format("~s\n", [?JSON(Missing)]),
  ok.

sort_deps(Files) ->
  {ok, Mods} = erl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),
  {ok, DepGraph} = lifter_depgraph:from_mods(Mods),
  case lifter_depgraph:topo_sort(DepGraph) of
    {ok, Sort} -> io:format("~s\n", [?JSON(Sort)]);
    {error, {found_cycles, Cycles}} ->
      io:format("~s\n", [?JSON(#{ dependency_cycles => Cycles })])
  end.


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


uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> lists:sort(sets:to_list(sets:from_list(lists:flatten(Xs), [{version, 2}]))).
