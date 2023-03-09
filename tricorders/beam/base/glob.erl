-module(glob).

-export([glob/1]).
-export([glob/2]).

glob(Glob) when is_binary(Glob) ->
    [path:new(P) || P <- filelib:wildcard(binary:bin_to_list(Glob))].

glob(Root, Glob) when is_binary(Glob) -> [path:strip_prefix(Root, P) || P <- glob(Glob)].
