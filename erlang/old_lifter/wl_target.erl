-module(wl_target).

-include_lib("kernel/include/logger.hrl").

-export([deps/1]).
-export([name/1]).
-export([set_deps/2]).
-export([srcs/1]).

name({_Rule, Spec}) -> maps:get(name, Spec).
srcs({_Rule, Spec}) -> maps:get(srcs, Spec, []).
deps({_Rule, Spec}) -> maps:get(deps, Spec, []).

set_deps(Spec, Deps) when is_list(Deps) -> maps:put(deps, Deps, Spec).
