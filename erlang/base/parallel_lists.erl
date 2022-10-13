-module(parallel_lists).

-export([map/2]).
-export([map/3]).

map(Fn, Ls) -> map(Fn, Ls, 1000).
map(Fn, Ls, Timeout) ->
  Self = self(),
  Idxs = lists:seq(0, length(Ls) - 1),
  LsIdx = lists:zip(Idxs, Ls),
  Pids = lists:map(fun ({Idx, Item}) ->
                       spawn(fun () -> Self ! {result, self(), Idx, Fn(Item)} end)
                   end, LsIdx),
  map_recv_loop(Pids, #{}, Timeout).

map_recv_loop([], Acc, _T)  -> maps:values(Acc);
map_recv_loop(Pids0, Acc0, Timeout) ->
  receive
    {result, Pid, Idx, Val} ->
      Pids = lists:delete(Pid, Pids0),
      Acc = maps:put(Idx, Val, Acc0),
      map_recv_loop(Pids, Acc, Timeout)
  after
    Timeout -> throw({parallel_map_timeout, Timeout, {partial_results, Acc0}})
  end.
