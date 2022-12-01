%% @doc The `erl_analyzer` analyzes a file by preprocessing and parsing it into
%% Erlang Syntax Forms, and then analyzing those forms.
%%
-module(erl_analyzer).

-include_lib("kernel/include/logger.hrl").

-export([analyze/1]).
-export([analyze/3]).

-export([subtree/2]).
-export([ast/1]).
-export([dependency_includes/1]).
-export([dependency_modules/1]).
-export([dependency_type_modules/1]).
-export([exports/1]).
-export([exported_functions/1]).
-export([exported_types/1]).
-export([functions/1]).
-export([includes/1]).
-export([missing_includes/1]).
-export([missing_modules/1]).
-export([modules/1]).
-export([parse_transforms/1]).

-export_type([err/0]).
-export_type([mod_desc/0]).
-export_type([opts/0]).

-opaque mod_desc() :: #{
                        ast => fun(() -> term()),
                        exports => #{ functions => [], types => [] },
                        functions => [],
                        includes => [],
                        missing_includes => [],
                        missing_modules => [],
                        modules => [],
                        parse_transforms => []
                       }.

-type err() :: {parse_error, term()}.

-type opts() :: #{ compiler_opts => [atom()] }.

%%--------------------------------------------------------------------------------------------------
%% Setters / Getters
%%--------------------------------------------------------------------------------------------------

ast(#{ ast := Fn }) -> Fn().
functions(#{ functions := Fns }) -> Fns.
exports(#{ exports := Ex }) -> Ex.
exported_functions(#{ exports := #{ functions := Fns }}) -> Fns.
exported_types(#{ exports := #{ types := Tys }}) -> Tys.
includes(#{ includes := X }) -> X.
missing_includes(#{ missing_includes := X }) -> X.
missing_modules(#{ missing_modules := X }) -> X.
modules(#{ modules := X }) -> X.
parse_transforms(#{ parse_transforms := X }) -> X.

%%--------------------------------------------------------------------------------------------------
%% Tree Splitter
%%--------------------------------------------------------------------------------------------------
subtree(#{ ast := Fn }, all) -> {ok, Fn()};
subtree(#{ ast := Fn }, {named, Sym}) -> 
  Ast = Fn(), 
  case get_symbol(Ast, Sym) of
    none -> {error, {symbol_not_found, Sym}};
    {some, Subtree} -> slice_tree(Ast, Subtree, Sym)
  end.

slice_tree(Ast, Symbol, Sym) ->
  AllDeps = get_sym_deps_rec(Ast, Symbol),
  Subtree = strip_tree(Ast, [Sym|AllDeps], []),
  {ok, Subtree}.

strip_tree([], _Symbols, Acc) -> lists:reverse(Acc);

strip_tree([{attribute, Loc, export, Exports0}|Ast], Symbols, Acc) ->
  Exports = lists:filter(fun (Export) -> lists:member(Export, Symbols) end, Exports0),
  case Exports of
    [] -> strip_tree(Ast, Symbols, Acc);
    _  -> strip_tree(Ast, Symbols, [{attribute, Loc, export, Exports}|Acc])
  end;

strip_tree([Node={attribute, _ , _, _}|Ast], Symbols, Acc) -> strip_tree(Ast, Symbols, [Node|Acc]);

strip_tree([Node={function, _Loc, Name, Arity, _Body}|Ast], Symbols, Acc) ->
  case lists:member({Name, Arity}, Symbols) of
    true -> 
      io:format("Found function: ~p/~p\n", [Name, Arity]),
      strip_tree(Ast, Symbols, [Node|Acc]);
    false -> 
      io:format("Skipped function: ~p/~p\n", [Name, Arity]),
      strip_tree(Ast, Symbols, Acc)
  end;

strip_tree([Node|Ast], Symbols, Acc) ->
  strip_tree(Ast, Symbols, [Node|Acc]).

get_symbol(Ast, {Sym, Args}) ->
  erl_visitor:walk(
    Ast,
    _Acc = none,
    fun
      (Ast={function, _Loc1, Name, Arity, _Body}, none) ->
        io:format("Searching for symbol: ~p/~p\n", [Sym, Args]),
        io:format("Found symbol: ~p/~p\n", [Name, Arity]),
        case (Name =:= Sym) and (Arity =:= Args) of
          true -> {some, Ast};
          false -> none
        end;
      (_Ast, Acc) ->
        Acc
    end).

get_sym_deps(Ast) ->
  skip_prelude(uniq(erl_visitor:walk(
                      Ast,
                      _Acc = [], 
                      fun
                        (_Ast = {call, _Loc, {atom, _Loc2, Name}, Args}, Acc) ->
                          [ {Name, length(Args)} | Acc ];
                        (_Ast, Acc) ->
                          Acc
                      end))).

get_sym_deps_rec(Ast, Symbol) ->
  NewDeps = get_sym_deps(Symbol),
  DepAsts = [ begin 
                {some, DepAst} = get_symbol(Ast, Dep),
                get_sym_deps_rec(Ast, DepAst)
              end || Dep <- NewDeps ],
  lists:flatten([NewDeps | DepAsts]).

skip_prelude(Fns) ->
	lists:filtermap(fun (Fn) ->
											case erl_stdlib:is_prelude_function(Fn) of
												true -> false;
												false -> {true, Fn}
											end
									end, Fns).

%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

-spec dependency_modules(mod_desc()) -> [atom() | binary()].
dependency_modules(#{ modules := Mods, missing_modules := MissingMods }) -> uniq(Mods ++ MissingMods);
dependency_modules(_) -> [].

-spec dependency_type_modules(mod_desc()) -> [atom() | binary()].
dependency_type_modules(#{ type_modules := TypeMods, missing_type_modules := MissingTypeMods }) -> uniq(TypeMods ++ MissingTypeMods);
dependency_type_modules(_) -> [].

-spec dependency_includes(mod_desc()) -> [atom() | binary()].
dependency_includes(#{ includes := Hrls, missing_includes := MissingHrls }) -> uniq(Hrls ++ MissingHrls);
dependency_includes(_) -> [].


-spec analyze([path:t()]) -> result:t(mod_desc(), term()).
analyze(Files) ->
  IncludePaths = include_paths(Files),
  ModMap = mod_map(Files),
  analyze(Files, ModMap, IncludePaths).

analyze(Files, ModMap, IncludePaths) ->
  Result = maps:from_list(lists:map(fun (File) -> do_analyze(File, IncludePaths, ModMap) end, Files)),
  {ok, Result}.

do_analyze(Path, IncludePaths, ModMap) ->
  {ok, Ast} = erl_ast:parse_file(Path, IncludePaths),
  RawIncludes = erl_ast:get_includes(Path),

  {Mods, MissingMods} = mods(Path, ModMap, Ast),

  {TypeMods, MissingTypeMods} = type_mods(Path, ModMap, Ast),

  ParseTrans = parse_trans(Ast),

  {Includes, MissingIncludes} = get_includes(Ast),

  {Functions, Exports} = case erl_stdlib:file_to_module(Path) of
                {ok, ModName} -> 
                  {all_functions(ModName, Ast), all_exports(ModName, Ast)};
                _ -> {[], #{ types => [], functions => [], extra => [] }}
              end,

  {Path, #{
           modules => Mods,
           type_modules => TypeMods,
           includes => Includes ++ RawIncludes,
           functions => Functions,
           exports => Exports,
           missing_includes => MissingIncludes,
           missing_modules => MissingMods,
           missing_type_modules => MissingTypeMods,
           parse_transforms => ParseTrans,
           ast => fun () -> Ast end
          }}.

all_exports(ModName, Ast) ->
  {ExportedFns, ExportedTypes} = erl_visitor:walk(
    Ast,
    _Acc = { _Fns = [], _Types = [] },
    fun
      (_Ast={attribute, _Loc1, export, Exports}, {Fns, Types}) ->
        {Exports ++ Fns, Types};

      (_Ast={attribute, _Loc1, export_type, TypeExports}, {Fns, Types}) ->
        {Fns, TypeExports ++ Types};

      (_Ast, Acc) ->
        Acc
    end),

  #{
    types => uniq([ {ModName, Name, Arity} || {Name, Arity} <- ExportedTypes ]),
    functions => uniq([ {ModName, Name, Arity} || {Name, Arity} <- ExportedFns ])
   }.

all_functions(ModName, Ast) ->
  AllFns = uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={function, _Loc1, Name, Arity, _Clauses}, Acc) ->
        [{ModName, Name, Arity} | Acc];

      (_Ast, Acc) ->
        Acc
    end)),

  AllFns.

parse_trans(Ast) ->
  AllMods = skip_std(uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={attribute, _Loc1, compile, Args}, Acc) when is_list(Args) ->
        case proplists:lookup(parse_transform, Args) of
          none -> Acc;
          {parse_transform, Mod} -> [Mod | Acc]
        end;
      (_Ast={attribute, _Loc1, compile, _Args={parse_transform, Mod}}, Acc) ->
        [Mod | Acc];
      (_Ast, Acc) ->
        Acc
    end))),

  AllMods.

type_mods(CurrPath, ModMap, Ast) ->
  AllMods = skip_std(uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={remote_type, _Loc1, [{atom, _Loc3, Mod}, _Fun, _Args]}, Acc) ->
        [Mod | Acc];

      (_Ast, Acc) ->
        Acc
    end))),

  {Mods, Missing} = lists:foldl(fun (Mod, {Mods, Missing}) ->
                             case maps:get(Mod, ModMap, missing) of
                               missing -> {Mods, [Mod|Missing]};
                               Path when Path =/= CurrPath -> {[Path|Mods], Missing};
                               _ -> {Mods, Missing}
                             end
                         end, {_Mods = [], _Missing = []}, AllMods),

  {uniq(Mods), uniq(Missing)}.

mods(CurrPath, ModMap, Ast) ->
  AllMods = skip_std(uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={attribute, _Loc1, import, _Args={Mod, _ModArgs}}, Acc) ->
        [Mod | Acc];

      (_Ast={attribute, _Loc1, compile, _Args={parse_transform, Mod}}, Acc) ->
        [Mod | Acc];

      (_Ast={remote, _Loc2, {atom, _Loc3, Mod}, _Fun}, Acc) ->
        [Mod | Acc];

      (_Ast={remote_type, _Loc1, [{atom, _Loc3, Mod}, _Fun, _Args]}, Acc) ->
        [Mod | Acc];

      (_Ast, Acc) ->
        Acc
    end))),

  {Mods, Missing} = lists:foldl(fun (Mod, {Mods, Missing}) ->
                             case maps:get(Mod, ModMap, missing) of
                               missing -> {Mods, [Mod|Missing]};
                               Path when Path =/= CurrPath -> {[Path|Mods], Missing};
                               _ -> {Mods, Missing}
                             end
                         end, {_Mods = [], _Missing = []}, AllMods),

  {uniq(Mods), uniq(Missing)}.


get_includes(Ast) ->
  {Includes, Missing} = erl_visitor:walk(
    Ast,
    {_Includes = [], _Missing = []},
    fun
      (_Ast={error, {_Loc1, epp, {include, file, File}}}, {Includes, Missing}) ->
        {Includes, [binary:list_to_bin(File) | Missing]};

      (_Ast={error, {_Loc1, epp, {include, lib, File}}}, {Includes, Missing}) ->
        {Includes, [binary:list_to_bin(File) | Missing]};

      (_Ast={attribute, _Loc1, file, {File, _Loc2}}, {Includes, Missing} = Acc) ->
        case filename:extension(File) of
          ".hrl" -> {[binary:list_to_bin(File) | Includes], Missing};
          _ -> Acc
        end;

      (_Ast={attribute, _Loc1, include, File}, {Includes, Missing}) ->
        {[binary:list_to_bin(File) | Includes], Missing};

      (_Ast={attribute, _Loc1, include_lib, File}, {Includes, Missing}) ->
        {[binary:list_to_bin(File) | Includes], Missing};

      (_Ast, Acc) ->
        Acc
    end),

  {uniq(Includes), uniq(Missing)}.

skip_std(Mods) ->
  lists:filtermap(fun
                    (Mod) when is_atom(Mod) ->
                      case erl_stdlib:is_user_module(Mod) of
                        true -> {true, Mod};
                        false -> false
                      end;
                    (Mod) -> {true, Mod}
                  end, Mods).

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).

mod_map(Paths) ->
  #{ sources := Sources } = source_tagger:tag(Paths),
  maps:from_list(([ {erlang:binary_to_atom(filename:basename(S, ".erl"), utf8), S} || S <- Sources ])).

include_paths(Paths) ->
  #{ headers := Headers } = source_tagger:tag(Paths),
  uniq([ binary:bin_to_list(filename:dirname(H)) || H <- Headers ]).
