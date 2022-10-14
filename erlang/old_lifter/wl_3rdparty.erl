-module(wl_3rdparty).

-include_lib("kernel/include/logger.hrl").

-export([buildfiles/2]).
-export([read_overrides/1]).

thirdparty_root() -> "3rdparty".

overrides_file() -> "rebar.overrides".
overrides_file(Path) -> filename:join([Path, thirdparty_root(), overrides_file()]).

%===============================================================================
% Read an overrides file from disk
%===============================================================================
read_overrides(Path) ->
  OverridesPath = overrides_file(Path),
  ?LOG_INFO("Reading overrides from ~p\n", [OverridesPath]),
  case file:consult(OverridesPath) of
    {ok, [Data]} -> {ok, Data};
    {error, enoent} -> {ok, #{}}
  end.

%===============================================================================
% Given a list of Rebar3/Erlang.mk projects, create the corresponding Buildfiles
%===============================================================================

buildfiles(Deps, Overrides) ->
  DepList = wl_dependency:to_list(Deps),
  {ok, Graph} = create_dep_graph(DepList),
  Targets = targets(Graph, Overrides),
  Buildfiles = collect_buildfiles(DepList, Targets),
  {ok, Buildfiles}.

collect_buildfiles(P, T) -> collect_buildfiles(P, T, []).
collect_buildfiles([], _, Acc) -> Acc;
collect_buildfiles([Proj|Rest], Targets, Acc) ->
  Root = wl_rebar_project:root(Proj),
  Name = wl_rebar_project:name(Proj),
  Target = maps:get(Name, Targets, undefined),
  Proj1 = {wl_buildfile:path(Root), Target},
  collect_buildfiles(Rest, Targets, [Proj1 | Acc]).

create_dep_graph(DepList) ->
  Graph = digraph:new(),
  ok = add_deps(Graph, DepList),
  {ok, Graph}.

add_deps(_Graph, []) -> ok;
add_deps(Graph, [Proj | Projects]) ->
  Name = wl_rebar_project:name(Proj),
  Deps = wl_rebar_project:deps(Proj),
  digraph:add_vertex(Graph, Name),
  lists:foreach(fun (D) -> add_one_dep(Graph, Name, D) end, Deps),
  add_deps(Graph, Projects).

add_one_dep(Graph, Name, Dep) ->
  DepName = wl_dependency:name(Dep),
  digraph:add_vertex(Graph, DepName),

  if DepName =/= Name ->
      case edges(Graph, Name) of
        [] -> digraph:add_edge(Graph, Name, DepName);
        Edges ->
          case edge_exists(Edges, DepName, Name) of
            true -> ok;
            false -> digraph:add_edge(Graph, Name, DepName)
          end
      end;
    true -> ok
  end.

edges(Graph, Name) ->
  EdgeIdxs = digraph:edges(Graph, Name),
  lists:map(fun (Idx) -> digraph:edge(Graph, Idx) end, EdgeIdxs).

edge_exists(Edges, DepName, Name) ->
  lists:any(fun
              ({_, A, B, _}) when (A == Name) and (B == DepName) -> true;
              ({_, A, B, _}) when (A == DepName) and (B == Name) -> true;
              (_) -> false
            end, Edges).

targets(Graph, Overrides) -> targets(Graph, Overrides, digraph:vertices(Graph), #{}).
targets(_Graph, _Overrides, [], Acc) -> Acc;
targets(Graph, Overrides, [Name|T], Acc) ->
  Deps = project_deps(Graph, Name),
  RuleName = choose_rule(Name),
  Target = target_with_overrides(Name, RuleName, Deps, Overrides),

  Targets = maps:put(RuleName, [ Target ], #{}),
  Acc1 = maps:put(Name, Targets, Acc),
  targets(Graph, Overrides, T, Acc1).

project_deps(Graph, Name) ->
  Edges = edges(Graph, Name),
  DepNames = lists:filtermap(fun
              ({_, A, B, _}) when (A == Name) -> {true, B};
              ({_, _, _, _}) -> false
            end, Edges),
  lists:sort(DepNames).

choose_rule(Name) ->
  Dir = wl_rebar3:libdir(Name),
  RebarConf = filename:join(Dir, wl_rebar3:config_file()),
  ErlMk = filename:join(Dir, "erlang.mk"),
  Make = filename:join(Dir, "Makefile"),
  Mix = filename:join(Dir, "mix.exs"),

  HasRebarConf = filelib:is_file(RebarConf),
  HasErlMk = filelib:is_file(ErlMk),
  HasMake = filelib:is_file(Make),
  HasMix = filelib:is_file(Mix),

  if
    HasRebarConf -> rebar3_library;
    HasErlMk -> erlangmk_library;
    HasMake -> make_library;
    HasMix -> mix_library;
    true -> rebar3_library
  end.

target_with_overrides(Name, _RuleName, Deps, Overrides) ->
  Target = #{ name => atom_to_binary(Name),
              deps => to_labels(Deps)
            },
  Override = maps:get(Name, Overrides, #{}),
  maps:merge(Target, Override).

to_labels([]) -> [];
to_labels([D|Rest]) ->
  {ok, Cwd} = file:get_cwd(),
  Root = wl_rebar3:libdir(),
  Path = binary:list_to_bin(string:prefix(filename:join(Root, D), Cwd)),
  Label = << "/", Path/binary >>,
  [ Label | to_labels(Rest) ].
