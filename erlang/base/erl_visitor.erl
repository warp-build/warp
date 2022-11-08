-module(erl_visitor).

-export([walk/3]).

walk([], Acc, _Fn) -> Acc;
walk([Node|Rest], Acc, Fn) ->
  walk(Rest, walk(Node, Acc, Fn), Fn);

walk(Node={'fun', _Ln, {'clauses', Clauses}}, Acc, Fn) ->
  lists:foldl(
    fun (Clause, ClauseAcc) -> walk(Clause, ClauseAcc, Fn) end,
    Fn(Node, Acc),
    Clauses);

walk(Node={'block', _Ln, Exprs}, Acc, Fn) ->
  lists:foldl(
    fun (Expr, ExprAcc) -> walk(Expr, ExprAcc, Fn) end,
    Fn(Node, Acc),
    Exprs);

walk(Node={'function', _Ln, _Name, _Arity, Clauses}, Acc, Fn) ->
  lists:foldl(
    fun (Clause, ClauseAcc) -> walk(Clause, ClauseAcc, Fn) end,
    Fn(Node, Acc),
    Clauses);

walk(Node={'clause', _Ln, _Args, _Guards, Exprs}, Acc, Fn) ->
  lists:foldl(
    fun (Expr, ExprAcc) -> walk(Expr, ExprAcc, Fn) end,
    Fn(Node, Acc),
    Exprs);

walk(Node={'match', _Ln, Expr1, Expr2}, Acc, Fn) ->
  Acc1 = Fn(Node, Acc),
  Acc2 = walk(Expr1, Acc1, Fn),
  walk(Expr2, Acc2, Fn);

walk(Node={'case', _Ln, Expr, Args}, Acc, Fn) ->
  Acc1 = Fn(Node, Acc),
  Acc2 = walk(Expr, Acc1, Fn),
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Acc2,
    Args);

walk(Node={'call', _Ln, _Name, Args}, Acc, Fn) ->
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Fn(Node, Acc),
    Args);

walk(Node={'attribute', _Ln, 'record', {_Name, Args}}, Acc, Fn) ->
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Fn(Node, Acc),
    Args);

walk(Node={'typed_record_field', _Name, Type}, Acc, Fn) ->
  Acc1 = Fn(Node, Acc),
  walk(Type, Acc1, Fn);

walk(Node={'type', _Ln, _Kind, Def}, Acc, Fn) when is_list(Def) ->
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Fn(Node, Acc),
    Def);

walk(Node={'bin', _Ln, Parts}, Acc, Fn) ->
  lists:foldl(
    fun (Part, PartsAcc) -> walk(Part, PartsAcc, Fn) end,
    Fn(Node, Acc),
    Parts);

walk(Node={'cons', _Ln, Head, Tail}, Acc, Fn) ->
  Acc0 = Fn(Node, Acc),
  Acc1 = Fn(Head, Acc0),
  Fn(Tail, Acc1);

walk(Node={'var', _Ln, _Name}, Acc, Fn) ->
  Fn(Node, Acc);

walk(Node={'op', _Ln, _Op, Lhs, Rhs}, Acc, Fn) ->
  Acc0 = Fn(Node, Acc),
  Acc1 = Fn(Lhs, Acc0),
  Acc2 = Fn(Rhs, Acc1),
  Acc2;

walk(Node={'remote_type', _Name, [_Mod, _Type, Args]}, Acc, Fn) ->
  lists:foldl(
    fun (Arg, ArgAcc) -> walk(Arg, ArgAcc, Fn) end,
    Fn(Node, Acc),
    Args);

walk(Node, Acc, Fn) -> Fn(Node, Acc).
