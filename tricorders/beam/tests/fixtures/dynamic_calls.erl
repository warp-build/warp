-module(dynamic_calls).

-export([foo/1]).

foo(A) -> erlang:A().
