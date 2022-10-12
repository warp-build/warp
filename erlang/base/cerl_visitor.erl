-module(cerl_visitor).

-export([new/0]).
-export_type([t/0]).

-record(visitor, { overrides = none }).

-opaque t() :: #visitor{ overrides :: atom() }.

new() -> #visitor{}.
