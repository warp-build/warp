-module(str).

-export([ends_with/2]).
-export([begins_with/2]).

-type t() :: binary() | list().

-spec ends_with(Str :: t(), Suffix :: t()) -> bool().
ends_with(Str, Suffix) ->
  case string:split(Str, Suffix) of
    [_Prefix, ""] -> true;
    [<<_Prefix/binary>>, <<>>] -> true;
    _ -> false
  end.

-spec begins_with(Str :: t(), Prefix :: t()) -> bool().
begins_with(Str, Prefix) ->
  case string:split(Str, Prefix) of
    ["", _Rest] -> true;
    [<<>>, <<_Rest/binary>>] -> true;
    _ -> false
  end.
