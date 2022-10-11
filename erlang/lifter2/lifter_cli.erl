-module(lifter_cli).

-export([analyze_files/2]).

analyze_files(_WorkspaceRoot, Files) ->
  {ok, Results} = cerl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),
  io:format("~s", [ jsone:encode(Results, [{indent, 2}, {space, 1}]) ]),
  ok.
