-module(hexpm).

-export([archive_url/1]).
-export([package_name/1]).
-export([parse_url/1]).
-export([pkg_to_url/1]).
-export([spec_to_url/1]).

package_name({Name, _}) -> Name.

pkg_to_url(Name) when is_atom(Name) -> pkg_to_url(erlang:atom_to_binary(Name));
pkg_to_url(Name) when is_binary(Name) ->
  << "https://hex.pm/packages/", Name/binary >>.

spec_to_url({ Name, Vsn }) when is_binary(Vsn) ->
  << (pkg_to_url(Name))/binary, "/", Vsn/binary >>.

parse_url(<< "https://hex.pm/packages/", Url/binary >>) when is_binary(Url) ->
  {ok, parse_url(Url, { _Name = <<>>, _Vsn = <<>> }, name)};
parse_url(_) -> {error, wrong_url_prefix}.

parse_url(<<>>, Acc, _) -> Acc;
parse_url(<< "/", Rest/binary >>, Acc, name) -> parse_url(Rest, Acc, vsn);
parse_url(<< Char/utf8, Rest/binary >>, { Name, Vsn }, name) ->
  parse_url(Rest, { << Name/binary, Char >>, Vsn }, name);
parse_url(<< Char/utf8, Rest/binary >>, { Name, Vsn }, vsn) ->
  parse_url(Rest, { Name,  << Vsn/binary, Char >> }, vsn).


archive_url({ Name, Vsn }) ->
  << "https://repo.hex.pm/tarballs/", Name/binary, "-", Vsn/binary, ".tar" >>.
