-module(erl_ast).

-export([parse_file/2]).
-export([get_includes/1]).

%% @doc Parse a String into an Erlang AST running the Preprocessor first.
parse_file(File, IncludePaths) when is_binary(File) ->
  {ok, Forms} = epp:parse_file(binary:bin_to_list(File), [{includes, IncludePaths}]).

%% @doc Parse a String into an Erlang AST running the Preprocessor first.
get_includes(File) when is_binary(File) ->
  {ok, Forms} = epp:parse_file(binary:bin_to_list(File), []),
  lists:filtermap(fun 
                    ({error, {_Loc1, epp, {include, file, File}}}) ->
                      {true, binary:list_to_bin(File)};

                    ({error, {_Loc1, epp, {include, lib, File}}}) ->
                      {true, binary:list_to_bin(File)};

                    ({attribute, _Loc1, file, {File, _Loc2}}) ->
                      case filename:extension(File) of
                        ".hrl" -> {true, binary:list_to_bin(File)};
                        _ -> false
                      end;

                    ({attribute, _Loc1, include, File}) ->
                      {true, binary:list_to_bin(File)};

                    ({attribute, _Loc1, include_lib, File}) ->
                      {true, binary:list_to_bin(File)};

                    (_Form) -> false
                  end, Forms).

