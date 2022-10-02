-module(main).

-export([main/1]).

main(Args) -> io:format("Hello, world! Here's the args: ~p", [Args]).
