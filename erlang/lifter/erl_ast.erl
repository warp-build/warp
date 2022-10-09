-module(erl_ast).

-export([parse/1]).


%% @doc Parse a String into an Erlang AST.
parse(Src) when is_binary(Src) ->
  Str = binary:bin_to_list(Src),
  {ok, Tokens, _} = erl_scan:string(Str),
  Forms = collect_forms(Tokens),
  AST = parse_forms(Forms),
  {ok, AST}.

collect_forms(Tokens) -> collect_forms(Tokens, [[]]).
collect_forms([], Acc) -> lists:reverse(Acc);

%% when we find a dot, put it into the current stack, and push a new stack on top
collect_forms([T={dot, _}|Rest], [Current|Acc]) ->
  NewCurrent = [],
  LastCurrent = lists:reverse([ T | Current ]),
  Acc1 = [ LastCurrent | Acc],
  collect_forms(Rest, [ NewCurrent | Acc1 ]);

%% push the current token into the current stack
collect_forms([T|Rest], [Current|Acc]) ->
  Current1 = [ T | Current ],
  collect_forms(Rest, [ Current1 | Acc]).

parse_forms([]) -> [];
parse_forms([Form|Rest]) ->
  case erl_parse:parse_form(Form) of
    {ok, Ast} -> [Ast | parse_forms(Rest)];
    _ -> parse_forms(Rest)
  end.
