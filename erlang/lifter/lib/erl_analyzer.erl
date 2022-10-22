%% @doc The `erl_analyzer` analyzes a file by preprocessing and parsing it into Erlang Syntax Forms,
%% and then analyzing those forms.
%%
-module(erl_analyzer).

-include_lib("kernel/include/logger.hrl").

-export([analyze/1]).
-export([analyze/3]).

-export([ast/1]).
-export([functions/1]).
-export([dependency_modules/1]).
-export([dependency_includes/1]).

-export_type([mod_desc/0]).
-export_type([err/0]).
-export_type([opts/0]).

-opaque mod_desc() :: #{
                        modules => [],
                        includes => [],
                        functions => [],
                        missing_includes => [],
                        missing_modules => [],
                        parse_transforms => [],
                        ast => fun(() -> term())
                       }.

-type err() :: {parse_error, term()}.

-type opts() :: #{ compiler_opts => [atom()] }.

%%--------------------------------------------------------------------------------------------------
%% Setters / Getters
%%--------------------------------------------------------------------------------------------------

functions(#{ functions := Fns }) -> Fns.
ast(#{ ast := Fn }) -> Fn().

%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

-spec dependency_modules(mod_desc()) -> [atom() | binary()].
dependency_modules(#{ modules := Mods, missing_modules := MissingMods }) -> uniq(Mods ++ MissingMods);
dependency_modules(_) -> [].

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

  {Mods, MissingMods} = mods(Path, ModMap, Ast),

  ParseTrans = parse_trans(Ast),

  {Includes, MissingIncludes} = includes(Ast),

  Functions = case erl_stdlib:file_to_module(Path) of
                {ok, ModName} -> all_functions(ModName, Ast);
                _ -> []
              end,

  {Path, #{
           modules => Mods,
           includes => Includes,
           functions => Functions,
           missing_includes => MissingIncludes,
           missing_modules => MissingMods,
           parse_transforms => ParseTrans,
           ast => fun () -> Ast end
          }}.

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
      (_Ast={attribute, _Loc1, compile, _Args={parse_transform, Mod}}, Acc) ->
        [Mod | Acc];
      (_Ast, Acc) ->
        Acc
    end))),

  AllMods.

mods(CurrPath, ModMap, Ast) ->
  AllMods = skip_std(uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={attribute, _Loc1, import, _Args={Mod, _ModArgs}}, Acc) ->
        [Mod | Acc];

      (_Ast={attribute, _Loc1, compile, _Args={parse_transform, Mod}}, Acc) ->
        [Mod | Acc];

      (_Ast={call, _Loc1, {remote, _Loc2, {atom, _Loc3, Mod}, _Fun}, _Args}, Acc) ->
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


includes(Ast) ->
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
