-module(erl_analyzer).

-export([analyze/1]).

analyze(Files) ->
  IncludePaths = include_paths(Files),
  ModMap = mod_map(Files),

  Result = maps:from_list(lists:map(fun (File) -> do_analyze(File, IncludePaths, ModMap) end, Files)),
  {ok, Result}.

do_analyze(Path, IncludePaths, ModMap) ->
  {ok, Ast} = erl_ast:parse_file(Path, IncludePaths),

  Mods = mods(ModMap, Ast),

  Includes = includes(Ast),

  {Path, #{ mods => Mods, includes => Includes }}.

mods(ModMap, Ast) ->
  AllMods = skip_std(uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={attribute, _Loc1, compile, _Args={parse_transform, Mod}}, Acc) ->
        [Mod | Acc];

      (_Ast={call, _Loc1, {remote, _Loc2, {atom, _Loc3, Mod}, _Fun}, _Args}, Acc) ->
        [Mod | Acc];

      (_Ast, Acc) ->
        Acc
    end))),
  uniq([ maps:get(Mod, ModMap, Mod) || Mod <- AllMods ]).


includes(Ast) ->
  uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={error, {_Loc1, epp, {include, lib, File}}}, Acc) ->
        [binary:list_to_bin(File) | Acc];

      (_Ast={attribute, _Loc1, include, File}, Acc) ->
        [binary:list_to_bin(File) | Acc];

      (_Ast={attribute, _Loc1, include_lib, File}, Acc) ->
        [binary:list_to_bin(File) | Acc];

      (_Ast, Acc) ->
        Acc
    end)).

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
  #{ sources := Sources } = tag_files(Paths),
  maps:from_list(([ {erlang:binary_to_atom(filename:basename(S, ".erl"), utf8), S} || S <- Sources ])).

include_paths(Paths) ->
  #{ headers := Headers } = tag_files(Paths),
  uniq([ binary:bin_to_list(filename:dirname(H)) || H <- Headers ]).

tag_files(Files) -> tag_files(Files,  [], [], []).
tag_files([], Srcs, Hdrs, Others) -> #{ sources => Srcs, headers => Hdrs, others => Others };
tag_files([<<"">>|Files], Srcs, Hdrs, Others) -> tag_files(Files, Srcs, Hdrs, Others);
tag_files([<<".">>|Files], Srcs, Hdrs, Others) -> tag_files(Files, Srcs, Hdrs, Others);
tag_files([File|Files], Srcs, Hdrs, Others) ->
  case (catch filename:extension(File)) of
    <<".erl">> -> tag_files(Files, [File|Srcs], Hdrs, Others);
    <<".hrl">> -> tag_files(Files, Srcs, [File|Hdrs], Others);
    _ -> tag_files(Files, Srcs, Hdrs, [File|Others])
  end.
