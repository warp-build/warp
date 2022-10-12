-module(lifter_cli).

-export([analyze_files/2]).
-export([sort_deps/1]).

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}])).

sort_deps(Files) ->
  {ok, Mods} = erl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),
  {ok, DepGraph} = lifter_depgraph:from_mods(Mods),
  {ok, Sort} = lifter_depgraph:topo_sort(DepGraph),
  io:format("~s\n", [?JSON(Sort)]),
  ok.


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
