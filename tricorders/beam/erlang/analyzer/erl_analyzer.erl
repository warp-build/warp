%% @doc The `erl_analyzer` analyzes a file by preprocessing and parsing it into
%% Erlang Syntax Forms, and then analyzing those forms.
%%
-module(erl_analyzer).

-include_lib("kernel/include/logger.hrl").

-export([analyze/1]).

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
    file => binary(),
    ast => term(),
    exports => #{functions => [], types => []},
    functions => [],
    includes => [],
    missing_includes => [],
    missing_modules => [],
    modules => [],
    parse_transforms => []
}.

-type err() :: {parse_error, term()}.

-type opts() :: #{compiler_opts => [atom()]}.

%%--------------------------------------------------------------------------------------------------
%% Setters / Getters
%%--------------------------------------------------------------------------------------------------

ast(#{ast := Ast}) -> Ast.
functions(#{functions := Fns}) -> Fns.
exports(#{exports := Ex}) -> Ex.
exported_functions(#{exports := #{functions := Fns}}) -> Fns.
exported_types(#{exports := #{types := Tys}}) -> Tys.
includes(#{includes := X}) -> X.
missing_includes(#{missing_includes := X}) -> X.
missing_modules(#{missing_modules := X}) -> X.
modules(#{modules := X}) -> X.
parse_transforms(#{parse_transforms := X}) -> X.

-spec dependency_modules(mod_desc()) -> [atom() | binary()].
dependency_modules(#{modules := Mods, missing_modules := MissingMods}) ->
    uniq(Mods ++ MissingMods);
dependency_modules(_) ->
    [].

-spec dependency_type_modules(mod_desc()) -> [atom() | binary()].
dependency_type_modules(#{type_modules := TypeMods, missing_type_modules := MissingTypeMods}) ->
    uniq(TypeMods ++ MissingTypeMods);
dependency_type_modules(_) ->
    [].

-spec dependency_includes(mod_desc()) -> [atom() | binary()].
dependency_includes(#{includes := Hrls, missing_includes := MissingHrls}) ->
    uniq(Hrls ++ MissingHrls);
dependency_includes(_) ->
    [].

%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

analyze(#{ast := Ast, file := File, include_paths := IncludePaths}) ->
    RawIncludes = erl_ast:get_includes(File),

    Mods = mods(File, Ast),

    TypeMods = type_mods(File, Ast),

    ParseTrans = parse_trans(Ast),

    {Includes, MissingIncludes} = get_includes(Ast),

    {Functions, Exports} =
        case erl_stdlib:file_to_module(File) of
            {ok, ModName} ->
                {all_functions(ModName, Ast), all_exports(ModName, Ast)};
            _ ->
                {[], #{types => [], functions => [], extra => []}}
        end,

    {ok, #{
        file => File,
        modules => Mods,
        type_modules => TypeMods,
        includes => Includes ++ RawIncludes,
        functions => Functions,
        exports => Exports,
        missing_includes => MissingIncludes,
        parse_transforms => ParseTrans,
        ast => Ast
    }}.

all_exports(ModName, Ast) ->
    {ExportedFns, ExportedTypes} = erl_visitor:walk(
        Ast,
        _Acc = {_Fns = [], _Types = []},
        fun
            (_Ast = {attribute, _Loc1, export, Exports}, {Fns, Types}) ->
                {Exports ++ Fns, Types};
            (_Ast = {attribute, _Loc1, export_type, TypeExports}, {Fns, Types}) ->
                {Fns, TypeExports ++ Types};
            (_Ast, Acc) ->
                Acc
        end
    ),

    #{
        types => uniq([{ModName, Name, Arity} || {Name, Arity} <- ExportedTypes]),
        functions => uniq([{ModName, Name, Arity} || {Name, Arity} <- ExportedFns])
    }.

all_functions(ModName, Ast) ->
    AllFns = uniq(
        erl_visitor:walk(
            Ast,
            _Acc = [],
            fun
                (_Ast = {function, _Loc1, Name, Arity, _Clauses}, Acc) ->
                    [{ModName, Name, Arity} | Acc];
                (_Ast, Acc) ->
                    Acc
            end
        )
    ),

    AllFns.

parse_trans(Ast) ->
    AllMods = skip_std(
        uniq(
            erl_visitor:walk(
                Ast,
                _Acc = [],
                fun
                    (_Ast = {attribute, _Loc1, compile, Args}, Acc) when is_list(Args) ->
                        case proplists:lookup(parse_transform, Args) of
                            none -> Acc;
                            {parse_transform, Mod} -> [Mod | Acc]
                        end;
                    (_Ast = {attribute, _Loc1, compile, _Args = {parse_transform, Mod}}, Acc) ->
                        [Mod | Acc];
                    (_Ast, Acc) ->
                        Acc
                end
            )
        )
    ),

    AllMods.

type_mods(CurrPath, Ast) ->
    AllMods = skip_std(
        uniq(
            erl_visitor:walk(
                Ast,
                _Acc = [],
                fun
                    (_Ast = {remote_type, _Loc1, [{atom, _Loc3, Mod}, _Fun, _Args]}, Acc) ->
                        [Mod | Acc];
                    (_Ast, Acc) ->
                        Acc
                end
            )
        )
    ),

    uniq(AllMods).

mods(CurrPath, Ast) ->
    AllMods = skip_std(
        uniq(
            erl_visitor:walk(
                Ast,
                _Acc = [],
                fun
                    (_Ast = {attribute, _Loc1, import, _Args = {Mod, _ModArgs}}, Acc) ->
                        [Mod | Acc];
                    (_Ast = {attribute, _Loc1, compile, _Args = {parse_transform, Mod}}, Acc) ->
                        [Mod | Acc];
                    (_Ast = {remote, _Loc2, {atom, _Loc3, Mod}, _Fun}, Acc) ->
                        [Mod | Acc];
                    (_Ast = {remote_type, _Loc1, [{atom, _Loc3, Mod}, _Fun, _Args]}, Acc) ->
                        [Mod | Acc];
                    (_Ast, Acc) ->
                        Acc
                end
            )
        )
    ),

    uniq(AllMods).

get_includes(Ast) ->
    {Includes, Missing} = erl_visitor:walk(
        Ast,
        {_Includes = [], _Missing = []},
        fun
            (_Ast = {error, {_Loc1, epp, {include, file, File}}}, {Includes, Missing}) ->
                {Includes, [binary:list_to_bin(File) | Missing]};
            (_Ast = {error, {_Loc1, epp, {include, lib, File}}}, {Includes, Missing}) ->
                {Includes, [binary:list_to_bin(File) | Missing]};
            (_Ast = {attribute, _Loc1, file, {File, _Loc2}}, {Includes, Missing} = Acc) ->
                case filename:extension(File) of
                    ".hrl" -> {[binary:list_to_bin(File) | Includes], Missing};
                    _ -> Acc
                end;
            (_Ast = {attribute, _Loc1, include, File}, {Includes, Missing}) ->
                {[binary:list_to_bin(File) | Includes], Missing};
            (_Ast = {attribute, _Loc1, include_lib, File}, {Includes, Missing}) ->
                {[binary:list_to_bin(File) | Includes], Missing};
            (_Ast, Acc) ->
                Acc
        end
    ),

    {uniq(Includes), uniq(Missing)}.

skip_std(Mods) ->
    lists:filtermap(
        fun
            (Mod) when is_atom(Mod) ->
                case erl_stdlib:is_user_module(Mod) of
                    true -> {true, Mod};
                    false -> false
                end;
            (Mod) ->
                {true, Mod}
        end,
        Mods
    ).

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).

mod_map(Paths) ->
    #{sources := Sources} = source_tagger:tag(Paths),
    maps:from_list(
        ([{erlang:binary_to_atom(filename:basename(S, ".erl"), utf8), S} || S <- Sources])
    ).

include_paths(Paths) ->
    #{headers := Headers} = source_tagger:tag(Paths),
    uniq([binary:bin_to_list(filename:dirname(H)) || H <- Headers]).
