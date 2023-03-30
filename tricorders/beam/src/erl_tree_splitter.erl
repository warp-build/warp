-module(erl_tree_splitter).

-include_lib("kernel/include/logger.hrl").

-export([split/1]).

-define(ERL_AST, 'Elixir.Tricorder.Analysis.Erlang.Ast').

split(#{file := File, include_paths := IncludePaths, symbol := Symbol}) ->
    {ok, Ast} = ?ERL_AST:parse(File, IncludePaths),
    io:format("~p\n", [Ast]),
    case ?ERL_AST:'has_include_errors?'(Ast) of
        {true, MissingIncludes} ->
            {ok, {missing_includes, MissingIncludes}};
        false ->
            {ok, Subtree} = subtree(Ast, Symbol),
            {ok, {completed, Subtree}}
    end.

%%--------------------------------------------------------------------------------------------------
%% Tree Splitter
%%--------------------------------------------------------------------------------------------------
subtree(Ast, all) ->
    {ok, Ast};
subtree(Ast, {named, Sym}) ->
    case get_symbol(Ast, Sym) of
        none -> {error, {symbol_not_found, Sym}};
        {some, Subtree} -> slice_tree(Ast, Subtree, Sym)
    end.

slice_tree(Ast, Symbol, Sym) ->
    AllDeps = get_sym_deps_rec(Ast, Symbol),
    Subtree = strip_tree(Ast, [Sym | AllDeps], []),
    {ok, Subtree}.

strip_tree([], _Symbols, Acc) ->
    lists:reverse(Acc);
strip_tree([{attribute, Loc, export, Exports0} | Ast], Symbols, Acc) ->
    Exports = lists:filter(fun(Export) -> lists:member(Export, Symbols) end, Exports0),
    case Exports of
        [] -> strip_tree(Ast, Symbols, Acc);
        _ -> strip_tree(Ast, Symbols, [{attribute, Loc, export, Exports} | Acc])
    end;
strip_tree([Node = {attribute, _, _, _} | Ast], Symbols, Acc) ->
    strip_tree(Ast, Symbols, [Node | Acc]);
strip_tree([Node = {function, _Loc, Name, Arity, _Body} | Ast], Symbols, Acc) ->
    case lists:member({Name, Arity}, Symbols) of
        true ->
            io:format("Found function: ~p/~p\n", [Name, Arity]),
            strip_tree(Ast, Symbols, [Node | Acc]);
        false ->
            io:format("Skipped function: ~p/~p\n", [Name, Arity]),
            strip_tree(Ast, Symbols, Acc)
    end;
strip_tree([Node | Ast], Symbols, Acc) ->
    strip_tree(Ast, Symbols, [Node | Acc]).

get_symbol(Ast, {Sym, Args}) ->
    erl_visitor:walk(
        Ast,
        _Acc = none,
        fun
            (Ast = {function, _Loc1, Name, Arity, _Body}, none) ->
                io:format("Searching for symbol: ~p/~p\n", [Sym, Args]),
                io:format("Found symbol: ~p/~p\n", [Name, Arity]),
                case (Name =:= Sym) and (Arity =:= Args) of
                    true -> {some, Ast};
                    false -> none
                end;
            (_Ast, Acc) ->
                Acc
        end
    ).

get_sym_deps(Ast) ->
    skip_prelude(
        uniq(
            erl_visitor:walk(
                Ast,
                _Acc = [],
                fun
                    (_Ast = {call, _Loc, {atom, _Loc2, Name}, Args}, Acc) ->
                        [{Name, length(Args)} | Acc];
                    (_Ast, Acc) ->
                        Acc
                end
            )
        )
    ).

get_sym_deps_rec(Ast, Symbol) ->
    NewDeps = get_sym_deps(Symbol),
    DepAsts = [
        case get_symbol(Ast, Dep) of
            {some, DepAst} -> get_sym_deps_rec(Ast, DepAst);
            none -> []
        end
     || Dep <- NewDeps
    ],
    lists:flatten([NewDeps | DepAsts]).

skip_prelude(Fns) ->
    lists:filtermap(
        fun(Fn) ->
            case erl_stdlib:is_prelude_function(Fn) of
                true -> false;
                false -> {true, Fn}
            end
        end,
        Fns
    ).

%%--------------------------------------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------------------------------------

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).
