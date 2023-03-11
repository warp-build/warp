-module(dummy_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) -> Forms.
