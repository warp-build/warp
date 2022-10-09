-module(wl_buildfile).

-include_lib("kernel/include/logger.hrl").

-export([new/1]).
-export([path/1]).
-export([deps/1]).
-export([srcs/1]).
-export([names/1]).

-export([merge/2]).
-export([merge_all/1]).
-export([write/2]).
-export([write_all/1]).

new(Map) when is_map(Map) -> Map.

merge_all(Bfs) -> merge_all(Bfs, #{}).
merge_all([], Acc) -> maps:to_list(Acc);
merge_all([{Path, Current}|Rest], Acc) ->
  Acc1 = maps:update_with(
    Path,
    fun (Last) -> merge(Last, Current) end,
    Current,
    Acc),
  merge_all(Rest, Acc1).


merge(A, B) -> merge_rules(maps:to_list(A) ++ maps:to_list(B), #{}).

merge_rules([], Acc) -> Acc;
merge_rules([{RuleName, Targets}|Rest], Acc) ->
  Acc2 = maps:update_with(
    RuleName,
    fun (LastTargets) -> lists:flatten(LastTargets ++ Targets) end,
    [Targets],
    Acc),
  merge_rules(Rest, Acc2).



deps({_Path, Buildfile}) -> map_targets(fun wl_target:deps/1, Buildfile).

srcs({_Path, Buildfile}) -> map_targets(fun wl_target:srcs/1, Buildfile).

names({_Path, Buildfile}) -> map_targets(fun wl_target:name/1, Buildfile).

map_targets(Fun, Buildfile) ->
  Rules = maps:to_list(Buildfile),
  Results = lists:flatmap(
    fun ({Rule, Targets}) ->
        lists:map(fun (Spec) ->
                      Target = {Rule, Spec},
                      Fun(Target)
                  end, Targets)
    end, Rules),
  sets:to_list(sets:from_list(Results)).



path(Path) when is_list(Path) -> filename:join(Path, "Build.toml").

write_all([]) -> ok;
write_all([{Path, Buildfile}|Rest]) ->
  ok = write(Path, Buildfile),
  write_all(Rest).

write(Path, B) ->
  ?LOG_INFO("Writing Build.toml at ~p", [Path]),
  Toml = toml:to_string(B),
  ok = file:write_file(Path, Toml).
