-module(lifter_cli).

-export([analyze_files/2]).

analyze_files(_WorkspaceRoot, Files) ->
  Results = lists:foldl(fun (FileName, Acc) -> 
                          {ok, Analysis} = cerl_analyzer:analyze(FileName),
                          maps:put(binary:list_to_bin(FileName), Analysis, Acc)
                      end, #{}, Files),
  io:format("~s", [ jsone:encode(Results, [{indent, 2}, {space, 1}]) ]),
  ok.
