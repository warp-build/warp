-module(tree_split_exports_multiple_matches).

-export([prop_a/1,foo_1_test/1, foo_2_test/1]).
-export([foo_3_test/1]).

prop_a(_) -> ok.
foo_1_test(_) -> bar.
foo_2_test(_) -> baz.
foo_3_test(_) -> baz.
