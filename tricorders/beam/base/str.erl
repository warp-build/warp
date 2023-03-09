-module(str).

-export([new/1]).
-export([ends_with/2]).
-export([begins_with/2]).

-type t() :: binary() | list().

new(Str) when is_binary(Str) -> Str;
new(Str) when is_list(Str) -> binary:list_to_bin(Str).

-spec ends_with(Str :: t(), Suffix :: t()) -> boolean().
ends_with(Str, Suffix) ->
    case string:split(Str, Suffix) of
        [_Prefix, ""] -> true;
        [<<_Prefix/binary>>, <<>>] -> true;
        _ -> false
    end.

-spec begins_with(Str :: t(), Prefix :: t()) -> boolean().
begins_with(Str, Prefix) ->
    case string:split(Str, Prefix) of
        ["", _Rest] -> true;
        [<<>>, <<_Rest/binary>>] -> true;
        _ -> false
    end.