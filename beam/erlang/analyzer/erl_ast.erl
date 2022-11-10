-module(erl_ast).

-export([parse_file/2]).

%% @doc Parse a String into an Erlang AST running the Preprocessor first.
parse_file(File, IncludePaths) when is_binary(File) ->
  {ok, Forms} = epp:parse_file(binary:bin_to_list(File), [{includes, IncludePaths}]).
