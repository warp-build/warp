-module(erl_visitor).

-export([walk/3]).

walk([], Acc, _Fn) -> Acc;
walk([Node|Rest], Acc, Fn) ->
  walk(Rest, walk(Node, Acc, Fn), Fn);

walk(Node={function, _Ln, _Name, _Arity, Clauses}, Acc, Fn) ->
  lists:foldl(
    fun (Clause, ClauseAcc) -> walk(Clause, ClauseAcc, Fn) end,
    Fn(Node, Acc),
    Clauses);

walk(Node={clause, _Ln, _Args, _Guards, Exprs}, Acc, Fn) ->
  lists:foldl(
    fun (Expr, ExprAcc) -> walk(Expr, ExprAcc, Fn) end,
    Fn(Node, Acc),
    Exprs);

walk(Node={'case', _Ln, Expr, Args}, Acc, Fn) ->
  Acc1 = Fn(Node, Acc),
  Acc2 = walk(Expr, Acc1, Fn),
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Acc2,
    Args);

walk(Node={call, _Ln, _Name, Args}, Acc, Fn) ->
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Fn(Node, Acc),
    Args);

walk(_Node, Acc, _Fn) -> Acc.
