-module(b).

-include("c.hrl").

-export([f/0]).

f() -> a:f(?MY_MACRO).

