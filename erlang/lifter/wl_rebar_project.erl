-module(wl_rebar_project).

-export([deps/1]).
-export([is_3rdparty_project/1]).
-export([is_user_project/1]).
-export([name/1]).
-export([root/1]).
-export([srcs/1]).

is_user_project(Proj) -> not is_3rdparty_project(Proj).

is_3rdparty_project({Root, _}) -> string:find(Root, wl_rebar3:basedir()) =/= nomatch.

root({Root, _}) -> Root.

name({_Root, Proj}) -> n(get_or_else(Proj, name, unknown_dep)).
n(Name) when is_list(Name) -> list_to_atom(Name);
n(Name) when is_binary(Name) -> binary_to_atom(Name, utf8);
n(Name) when is_atom(Name) -> Name.

deps({_Root, Proj}) -> get_or_else(Proj, deps, []).

srcs({_Root, Proj}) ->
  maps:merge(
    #{ srcs => [], includes => [], priv => [], tests => [] },
    get_or_else(Proj, srcs, #{})).

get_or_else(Proplist, Key, Default) ->
  % NOTE(@ostera): remember tuples are 1-indexed!
  case lists:keyfind(Key, 1, Proplist) of
    {Key, Value} -> Value;
    _ -> Default
  end.

