-module(tree_split_exports).

-export([a/1]).
-export([a/2]).
-export([a/3]).
-export([a/4]).
-export([a/5]).
-export([foo/0]).
-export([foo/1]).

a(_) -> ok.
a(_, _) -> ok.
a(_, _, _) -> ok.
a(_, _, _, _) -> ok.
a(_, _, _, _, _) -> ok.

foo() -> bar.
foo(_) -> baz.
