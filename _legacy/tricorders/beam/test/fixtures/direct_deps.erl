-module(direct_deps).

-export([f/0]).

f() -> dep_a:f().
g() -> dep_b:f().
h() -> dep_c:f().
