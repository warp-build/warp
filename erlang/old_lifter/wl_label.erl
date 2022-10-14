-module(wl_label).

-include_lib("kernel/include/logger.hrl").

-export([new_rebar_lib/1]).
-export([from_path/1]).
-export([to_path/1]).
-export([to_string/1]).

to_string(T) -> binary:list_to_bin(T).

to_path(T) ->
  [_ | Path] = filename:split(T),
  filename:join(Path).

from_path(Path) ->
  {ok, Cwd} = file:get_cwd(),
  "//" ++ filename:join(lists:subtract(filename:split(Path), filename:split(Cwd))).

new_rebar_lib(Name) when is_atom(Name) -> new_rebar_lib(erlang:atom_to_list(Name));
new_rebar_lib(Name) when is_list(Name) ->
  from_path(filename:join(wl_rebar3:libdir(), Name)).
