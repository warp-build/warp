-module(wl_dep_analyzer).

-include_lib("kernel/include/logger.hrl").

-export([find_cycles/1]).

find_cycles(Buildfiles) -> find_cycles(Buildfiles, 0).

find_cycles(Buildfiles, Iter) when Iter > 10 -> {ok, Buildfiles};
find_cycles(Buildfiles, Iter) ->
  ?LOG_INFO("Resolving and fixing cycles..."),

  Graph = digraph:new(),
  ok = add_files(Graph, Buildfiles),
  Cycles = digraph_utils:cyclic_strong_components(Graph),

  case Cycles of
    [] -> {ok, Buildfiles};
    _ ->
      ?LOG_INFO("Found cycles: ~p\n", [Cycles]),
      BundledTargets = bundle_cyclic_targets(Cycles, Buildfiles),
      NewBuildfiles = wl_buildfile:merge_all(BundledTargets),
      find_cycles(NewBuildfiles, Iter + 1)
  end.

add_files(_Graph, []) -> ok;
add_files(Graph, [{Path, _Buildfile = #{ erlang_test := Targets } } | Rest]) ->
  add_targets(Graph, Path, Targets),
  add_files(Graph, Rest);
add_files(Graph, [{Path, _Buildfile = #{ erlang_library := Targets } } | Rest]) ->
  add_targets(Graph, Path, Targets),
  add_files(Graph, Rest).

add_targets(Graph, Path, Targets) ->
  Label = wl_label:from_path(filename:dirname(Path)),
  digraph:add_vertex(Graph, Label),
  lists:foreach(fun (#{ deps := Deps }) ->
                    lists:foreach(fun (D) ->
                                      D2 = binary:bin_to_list(D),
                                      digraph:add_vertex(Graph, D2),
                                      digraph:add_edge(Graph, Label, D2)
                                  end, Deps)
                end, Targets).

handle_cycles([]) -> ok;
handle_cycles(Cycles) ->
  ?LOG_INFO("Oops! We found cycles in your code in the following packages:"),
  [ ?LOG_INFO("- ~p\n", [Cycle]) || Cycle <- Cycles ],
  ?LOG_INFO("Check the sources and break those cycles to continue."),
  ?LOG_INFO("You can call the same command again afterwards to see if the cycles are fixed!"),
  {error, {found_cycles, Cycles}}.

bundle_cyclic_targets([], Buildfiles) -> Buildfiles;
bundle_cyclic_targets(Cycles, Buildfiles) -> bundle(Cycles, Buildfiles, []).

bundle([], Buildfiles, Acc) -> Buildfiles ++ Acc;
bundle([Cycle|Rest], Buildfiles, Acc) ->
  {FilesWithCycles, Buildfiles1} =
    lists:partition(
      fun ({Path, _}) -> 
          Label = wl_label:from_path(filename:dirname(Path)),
          lists:member(Label, Cycle) end,
      Buildfiles),
  NewBuildfile = {NewPath, _ } = merge(Cycle, FilesWithCycles),
  Buildfiles2 = replace_label(NewPath, FilesWithCycles, Buildfiles1),
  bundle(Rest, Buildfiles2, [ NewBuildfile | Acc ]).

merge(Cycle, Buildfiles) ->
  {ok, CommonPath} = common_path(Cycle),
  Deps = all_deps(Buildfiles),
  Srcs = all_srcs(Buildfiles),
  Names = combine_names(Cycle),
  Lib = #{ name => Names,
           srcs => Srcs,
           deps => Deps },
  Target = #{ erlang_library => [ Lib ] },
  {wl_buildfile:path(CommonPath), Target}.

common_path([P|_]=Cycle) ->
  common_path(filename:split(P), [ filename:split(C) || C <- Cycle ]).

common_path([], Paths) -> {error, {no_common_prefix, Paths}};
common_path(["/" | Prefix], []) -> {ok, Prefix};
common_path(Prefix, Paths = [P|Rest]) ->
  case lists:prefix(Prefix, P) of
    true -> common_path(Prefix, Rest);
    false -> common_path(lists:droplast(Prefix), Paths)
  end.

all_deps(Buildfiles) ->
  Deps = lists:flatten(lists:flatmap(fun wl_buildfile:deps/1, Buildfiles)),
  sets:to_list(sets:from_list(Deps)).

all_srcs(Buildfiles) ->
  Srcs = lists:flatten(lists:flatmap(fun wl_buildfile:srcs/1, Buildfiles)),
  sets:to_list(sets:from_list(Srcs)).

combine_names(Cycles) ->
  Names = lists:map(fun (Path) -> 
                        case lists:nthtail(2, filename:split(Path)) of
                          [] -> "repo_root";
                          Parts -> filename:join(Parts) 
                        end
                    end, Cycles),
  binary:list_to_bin(string:join(Names, "+")).


replace_label(NewPath, FilesWithCycles, Buildfiles) ->
  Labels = lists:map(fun ({Path, _}) -> wl_label:to_string(wl_label:from_path(filename:dirname(Path))) end, FilesWithCycles),
  NewLabel = wl_label:to_string(wl_label:from_path(filename:dirname(NewPath))),

  lists:map(
    fun ({Path, Rules}) ->
        Rules1 = maps:map(
                   fun (Rule, Targets) ->
                       lists:map(fun (Spec) ->
                                     Deps = wl_target:deps({Rule, Spec}),
                                     Deps1 = lists:filtermap(
                                               fun (Dep) ->
                                                   case lists:member(Dep, Labels) of
                                                     false -> true;
                                                     true -> {true, NewLabel}
                                                   end
                                               end, Deps),
                                     Deps2 = sets:to_list(sets:from_list(Deps1)),
                                     wl_target:set_deps(Spec, Deps2)
                                 end, Targets)
                   end, Rules),
        {Path, Rules1}
    end, Buildfiles).

