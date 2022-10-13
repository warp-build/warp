-module(erl_analyzer).

-export([analyze/1]).

analyze(Files) ->
  IncludePaths = include_paths(Files),
  ModMap = mod_map(Files),

  Result = maps:from_list(lists:map(fun (File) -> do_analyze(File, IncludePaths, ModMap) end, Files)),
  {ok, Result}.

do_analyze(Path, IncludePaths, ModMap) ->
  {ok, Ast} = erl_ast:parse_file(Path, IncludePaths),

  {Mods, MissingMods} = mods(ModMap, Ast),

  {Includes, MissingIncludes} = includes(Ast),

  {Path, #{
           modules => Mods,
           includes => Includes,
           missing_includes => MissingIncludes,
           missing_modules => MissingMods
          }}.

mods(ModMap, Ast) ->
  AllMods = skip_std(uniq(erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
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
                               Path -> {[Path|Mods], Missing}
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
