-module(wl_dep_graph).

-include_lib("kernel/include/logger.hrl").

-export([buildfiles/1]).

%===============================================================================
% Generate build files for all sources in the repository
%===============================================================================

buildfiles(Projects) ->
  ProjList = wl_dependency:to_list(Projects),
  {LocalPkgList, ThirdpartyPkgList} =
    lists:partition(fun wl_rebar_project:is_user_project/1, ProjList),

  Srcs = all_sources(LocalPkgList),
  Tests = all_tests(LocalPkgList),
  ?LOG_INFO("Found ~p sources and ~p tests", [length(Srcs), length(Tests)]),

  SrcGroups = group_sources(Srcs),

  DepSrcs = all_sources(ThirdpartyPkgList),
  DepSrcGroups = group_sources(DepSrcs),

  FlatSrcGroups =
    lists:flatmap(
      fun ({PkgPath, PkgSrcs}) ->
          SrcMods = erl_stdlib:files_to_modules(PkgSrcs),
          [ {Mod, wl_label:from_path(PkgPath)} || Mod <- SrcMods ]
      end,
      maps:to_list(SrcGroups))
    ++ lists:flatmap(
         fun ({PkgPath, PkgSrcs}) ->
             BaseDirParts = filename:split(wl_rebar3:basedir()),
             PkgPathParts = filename:split(PkgPath),
             RelPkgParts = lists:subtract(PkgPathParts, BaseDirParts),
             PkgName = case RelPkgParts of
                         ["default", "plugins", Name | _] -> Name;
                         ["default", "lib", Name | _] -> Name
                       end,
             Label = wl_label:new_rebar_lib(PkgName),
             SrcMods = erl_stdlib:files_to_modules(PkgSrcs),
             [ {Mod, Label} || Mod <- SrcMods ]
         end,
         maps:to_list(DepSrcGroups)),
  Mod2Pkg = maps:from_list(FlatSrcGroups),

  SrcGroupsWithDeps = analyze_deps(SrcGroups, Mod2Pkg),

  SrcBuildfiles =
    lists:map(
      fun ({Label, DepLabels, _Srcs, _RootPath}) ->
          Path = wl_label:to_path(Label),
          Deps =[ wl_label:to_string(Dep) || Dep <- DepLabels ],
          Lib = #{ name => binary:list_to_bin(filename:basename(Path)),
                   deps => Deps },
          Target = #{ erlang_library => [ Lib ] },
          {wl_buildfile:path(Path), Target}
      end,
      SrcGroupsWithDeps),

  TestWithDeps = analyze_test_deps(Tests, Mod2Pkg),
  TestBuildfiles =
    lists:map(
      fun ({Label, DepLabels, [TestFile], RootPath}) ->
          io:format("~p ~p\n", [RootPath, TestFile]),
          Path = wl_label:to_path(Label),
          Deps =[ wl_label:to_string(Dep) || Dep <- DepLabels ],
          Lib = #{ name => binary:list_to_bin(filename:basename(TestFile, ".erl")),
                   test => binary:list_to_bin(filename:join(lists:subtract(filename:split(TestFile), filename:split(RootPath)))),
                   deps => Deps },
          Target = #{ erlang_test => [ Lib ] },
          {wl_buildfile:path(Path), Target}
      end,
      TestWithDeps),

  {ok, wl_buildfile:merge_all(SrcBuildfiles ++ TestBuildfiles)}.

all_tests(Projs) -> all_tests(Projs, []).
all_tests([], Acc) -> lists:sort(Acc);
all_tests([P|Rest], Acc) ->
  #{ tests := Tests } = wl_rebar_project:srcs(P),
  Acc1 = Tests ++ Acc,
  all_tests(Rest, Acc1).

all_sources(Projs) -> all_sources(Projs, []).
all_sources([], Acc) -> lists:sort(Acc);
all_sources([P|Rest], Acc) ->
  #{ srcs := Srcs,
     includes := Includes,
     priv := _Privs
   } = wl_rebar_project:srcs(P),
  Acc1 = Srcs ++ Includes ++ Acc,
  all_sources(Rest, Acc1).

group_sources(Srcs) -> group_sources(Srcs, #{}).
group_sources([], Acc) -> maps:filter(fun (_K, Srcs) -> length(Srcs) > 0 end, Acc);
group_sources([Src|Rest], Acc) ->
  Dir = filename:dirname(Src),
  Acc1 = maps:update_with(Dir, fun (Srcs) -> [Src|Srcs] end, [Src], Acc),
  group_sources(Rest, Acc1).


analyze_test_deps(Tests, Mod2Pkg) ->
  TestWithPath = lists:map(fun (TestSrc) -> { filename:dirname(TestSrc), [TestSrc]} end, Tests),
  analyze_deps(TestWithPath, Mod2Pkg).

analyze_deps(SrcGroups, Mod2Pkg) when is_map(SrcGroups) ->
  analyze_deps(maps:to_list(SrcGroups), Mod2Pkg);

analyze_deps(SrcGroups, Mod2Pkg) when is_list(SrcGroups) ->
  lists:map(
    fun ({Path, Srcs}) ->
        Label = wl_label:from_path(Path),
        io:format("Analyzing sources in ~p...\n", [Label]),
        Mods = lists:flatmap(fun analyze_dep/1, Srcs),
        Mods1 = lists:filter(fun erl_stdlib:is_user_module/1, Mods),
        Mods2 = skip_group_deps(Mods1, Srcs),
        Pkgs = find_packages(Mods2, Mod2Pkg),
        {Label, Pkgs, Srcs, Path}
    end, SrcGroups).

analyze_dep(Path) ->
  io:format("Analyzing ~p...\n", [Path]),
  {ok, Src} = file:read_file(Path),
  {ok, Ast} = erl_ast:parse(Src),
  Mods = mods(Ast),
  Mods.

mods(Ast) ->
  erl_visitor:walk(
    Ast,
    _Acc = [],
    fun
      (_Ast={call, _Loc1, {remote, _Loc2, {atom, _Loc3, Mod}, _Fun}, _Args}, Acc) ->
        [Mod | Acc];

      (_Ast, Acc) -> Acc
    end).

skip_group_deps(Mods, Srcs) ->
  SrcMods = erl_stdlib:files_to_modules(Srcs),
  lists:filter(fun (Mod) -> not lists:member(Mod, SrcMods) end, Mods).

find_packages(Mods, Mod2Pkg) ->
  Pkgs = lists:map(
    fun (Mod) -> maps:get(Mod, Mod2Pkg, wl_label:new_rebar_lib(Mod)) end,
    Mods),
  lists:sort(sets:to_list(sets:from_list(Pkgs))).


