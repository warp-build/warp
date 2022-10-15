-module(lifter_depgraph).

-include_lib("kernel/include/logger.hrl").

-export([topo_sort/1]).
-export([from_mods/1]).
-export([sort_files/1]).

-export_type([t/0]).

-opaque t() :: #{ graph => digraph:t() }.

-spec new() -> t().
new() -> #{ graph => digraph:new() }.

-spec add_mod(t(), atom()) -> ok.
add_mod(#{ graph := G }, Mod) ->
  digraph:add_vertex(G, Mod),
  ok.

-spec add_dep(t(), atom(), atom()) -> ok.
add_dep(T=#{ graph := G }, Mod, Dep) ->
  ok = add_mod(T, Mod),
  ok = add_mod(T, Dep),
  digraph:add_edge(G, Mod, Dep),
  ok.

-spec topo_sort(t()) -> result:t([atom()], {found_cycles, term()}).
topo_sort(#{ graph := G }) ->
  Cycles = digraph_utils:cyclic_strong_components(G),
  case Cycles of
    [] -> 
      Topo = digraph_utils:topsort(G),
      {ok, lists:reverse(Topo)};
    Reason ->
      {error, {found_cycles, Reason}}
  end.

-spec from_mods(map()) -> t().
from_mods(Mods) when is_map(Mods) ->
  Entries = maps:to_list(Mods),

  DepGraph = new(),

  lists:foreach(
    fun ({Mod, #{
                 modules := Modules,
                 includes := Includes,
                 missing_includes := MissingIncludes,
                 missing_modules := MissingModules
                }}) ->
        add_mod(DepGraph, Mod),
        ?LOG_DEBUG("add_mod(~p)\n", [Mod]),
        lists:foreach(fun 
                        (Dep) when Dep =:= Mod ->
                          ?LOG_DEBUG("SKIP add_dep(~p, ~p)\n", [Mod, Dep]);
                        (Dep) ->
                          ?LOG_DEBUG("add_dep(~p, ~p)\n", [Mod, Dep]),
                          add_dep(DepGraph, Mod, Dep)
                      end, Modules ++ Includes ++ MissingIncludes ++ MissingModules)
    end, Entries),

  {ok, DepGraph}.

-spec sort_files([path:t()]) -> [atom() | binary()].
sort_files(Files) ->
  {ok, Mods} = erl_analyzer:analyze(Files),
  {ok, DepGraph} = from_mods(Mods),
  topo_sort(DepGraph).
