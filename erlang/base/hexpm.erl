-module(hexpm).

-export([spec_to_url/1]).

spec_to_url({ Name, Vsn }) when is_binary(Vsn) ->
  << "https://hex.pm/packages/", Name/binary, "/", Vsn/binary >>.
