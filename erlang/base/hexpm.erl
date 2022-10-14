-module(hexpm).

-export([pkg_to_url/1]).
-export([spec_to_url/1]).

pkg_to_url(Name) when is_atom(Name) -> pkg_to_url(erlang:atom_to_binary(Name));
pkg_to_url(Name) when is_binary(Name) ->
  << "https://hex.pm/packages/", Name/binary >>.

spec_to_url({ Name, Vsn }) when is_binary(Vsn) ->
  << (pkg_to_url(Name))/binary, "/", Vsn/binary >>.
