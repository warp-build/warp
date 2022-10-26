-module(main).

-export([main/1]).

main(_Args) ->
  io:format("b:f() -> ~p\n", [b:f()]).
