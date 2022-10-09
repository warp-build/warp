-module(wl_dependency).

-include_lib("kernel/include/logger.hrl").

-export([all_deps/1]).
-export([name/1]).
-export([to_list/1]).
-export([to_rebar_deps_string/1]).
-export([version/1]).

to_rebar_deps_string(Deps) ->
  binary:list_to_bin(lists:flatten(io_lib:format("~p", [maps:values(Deps)]))).

to_list(Deps) -> lists:sort(maps:to_list(Deps)).

%===============================================================================
% Extract and flatten all dependencies for a list of projects
%===============================================================================

all_deps(Projects) ->
  ProjList = maps:to_list(Projects),
  AllDeps = lists:flatten(lists:flatmap(fun wl_rebar_project:deps/1, ProjList)),
  NonLocalDeps = lists:filter(fun is_3rdparty/1, AllDeps),
  SortedDeps = lists:sort(sets:to_list(sets:from_list(NonLocalDeps))),
  ?LOG_INFO("Deduplicating dependencies..."),
  {ok, UniqueDeps} = dedupe_deps(SortedDeps),
  {ok, UniqueDeps}.

is_3rdparty({_Name, {path, _Path}}) -> false;
is_3rdparty(Dep) when is_atom(Dep) -> false;
is_3rdparty(_) -> true.

dedupe_deps(Deps) -> dedupe_deps(Deps, #{}).
dedupe_deps([], Acc) -> {ok, Acc};
dedupe_deps([D|T], Acc0) ->
  Name = name(D),
  Dep = choose_dep(D, maps:get(Name, Acc0, none)),
  Acc1 = maps:put(Name, Dep, Acc0),
  dedupe_deps(T, Acc1).

choose_dep(A, none) -> A;
choose_dep(A, B) ->
  case compare_v(version(A), version(B)) of
    lt -> B;
    _ -> A
  end.

compare_v({override, _}, _) -> gt;
compare_v(_, {override, _}) -> lt;
compare_v({semver, A}, {semver, B}) -> verl:compare(A, B).

%===============================================================================
% Getters
%===============================================================================

name(Dep) when is_tuple(Dep) -> name(element(1, Dep));
name(Name) when is_list(Name) -> list_to_atom(Name);
name(Name) when is_binary(Name) -> binary_to_atom(Name, utf8);
name(Name) when is_atom(Name) -> Name.

version(Dep) ->
  RawV = binary:list_to_bin(v(Dep)),
  CleanV = clean_v(RawV),
  case verl:parse(CleanV) of
    {ok, V} -> {semver, V};
    _ -> {override, RawV}
  end.
v({_Name, {git, _Repo, {tag, Version}}}) -> Version;
v({_Name, {git, _Repo, {branch, Version}}}) -> Version;
v({_Name, {path, _Path}}) -> "0.0.0";
v({_Name, Version}) when is_binary(Version) -> Version;
v({_Name, Version}) when is_list(Version) -> Version.

clean_v(<<"v", Version/binary>>) -> Version;
clean_v(Version) -> Version.
