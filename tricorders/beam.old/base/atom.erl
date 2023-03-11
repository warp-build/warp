-module(atom).

-export([new/1]).

new(Name) when is_list(Name) -> erlang:list_to_atom(Name);
new(Name) when is_binary(Name) -> erlang:binary_to_atom(Name, ut8).
