-module(tree_split_exports).

-export([a/1,foo/0, foo/1]).

a(_) -> ok.
foo() -> bar.
foo(_) -> baz.
