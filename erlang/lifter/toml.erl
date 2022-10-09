-module(toml).

-include_lib("kernel/include/logger.hrl").

-export([to_string/1]).

to_string(Term) -> binary:list_to_bin(pp(Term, top)).

pp(X) -> pp(X, inline).

pp(X, _) when is_atom(X) -> atom_to_binary(X);
pp(X, _) when is_binary(X) -> ["\"", X, "\""];
pp(X, _) when is_number(X) -> X;

pp(X, top) when is_map(X) ->
  lists:map(
    fun
      ({K, V}) when is_list(V) ->
        lists:map(
          fun (TopV) ->
              ["[[", pp(K), "]]", "\n", pp(TopV, flat), "\n", "\n"]
          end,
          V);

      ({K, V}) -> [pp(K), " = ", pp(V), "\n"]
    end,
    maps:to_list(X));

pp(X, flat) when is_map(X) ->
  lists:map(fun ({K, V}) -> [pp(K), " = ", pp(V), "\n"] end,
            maps:to_list(X));

pp(X, inline) when is_map(X) ->
  KV = lists:map(fun ({K, V}) -> [pp(K), " = ", pp(V)] end,
                 maps:to_list(X)),
  lists:join("\n", ["{", lists:join(",", KV), "}"]);

pp([], top) -> "";

pp([], inline) -> "[]";
pp(Xs, inline) when is_list(Xs) ->
  lists:join(
    "\n",
    ["[", lists:join(",\n", lists:map(fun (X) -> ["  ", pp(X)] end, Xs)), "]"]).
