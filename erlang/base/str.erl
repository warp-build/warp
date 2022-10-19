-module(str).

-export([ends_with/2]).

-spec ends_with(binary(), binary()) -> bool().
ends_with(Str, Suffix) ->
  case string:split(Str, Suffix) of
    [_Prefix, ""] -> true;
    [<<_Prefix/binary>>, <<>>] -> true;
    _ -> false
  end.
