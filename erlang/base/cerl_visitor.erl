-module(cerl_visitor).

-record(visitor, { overrides = none }).

-opaque t() :: #visitor{ overrides :: atom() }.

new() -> #visitor{}.
