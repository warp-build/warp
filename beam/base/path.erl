-module(path).

-export([add_extension/2]).
-export([contains/2]).
-export([dirname/1]).
-export([ensure/1]).
-export([extension/1]).
-export([filename/1]).
-export([from_list/1]).
-export([is_prefix/2]).
-export([join/2]).
-export([new/1]).
-export([nth_tail/2]).
-export([relativize/1]).
-export([strip_prefix/2]).
-export([tail/1]).
-export([to_list/1]).
-export([to_string/1]).

-export_type([t/0]).

-opaque t() :: binary().

new(Str) ->
  case (catch unsafe_new(Str)) of
    {'ERROR', _} -> {error, invalid_input};
    Bin -> {ok, Bin}
  end.

unsafe_new(Str) -> erlang:iolist_to_binary([Str]).

-spec is_prefix(Prefix :: t(), Path :: t()) -> boolean().
is_prefix(Prefix, Path) -> strip_prefix(Prefix, Path) =/= Path.

-spec nth_tail(Path :: t(), N :: non_neg_integer()) -> t().
nth_tail(Path, N) when N > 0 ->
  Parts = to_list(Path),
  from_list(lists:nthtail(length(Parts) - N, Parts)).

-spec strip_prefix(Prefix :: t(), Path :: t()) -> t().
strip_prefix(Prefix, Path) ->
  Replaced = str:new(string:replace(Path, Prefix, "")),
  Final = case str:begins_with(Replaced, "/") of
            true -> str:new(string:replace(Replaced, "/", ""));
            false -> Replaced
          end,
  unsafe_new([ Final ]).

join(A, B) -> from_list([A, B]).

extension(Path) -> 
  case (catch filename:extension(unsafe_new(Path))) of
    {'ERROR', _} -> {error, no_extension};
    Ext -> {ok, Ext}
  end.

add_extension(Path, Ext) -> unsafe_new([Path, ".", Ext]).

filename(Path) -> unsafe_new(filename:basename(Path)).

dirname(Path) -> unsafe_new(filename:dirname(Path)).

contains(Path, Str) -> not (string:find(Path, Str) == nomatch).

to_list(Path) -> [ unsafe_new(Seg) || Seg <- filename:split(Path) ].

to_string(Path) -> binary:bin_to_list(Path).

from_list([]) -> unsafe_new("");
from_list(Ls) when is_list(Ls) -> unsafe_new(filename:join(Ls)).

-spec relativize(Path :: t()) -> t().
relativize(Path) ->
  case to_list(Path) of
    [<<".">> | Rest] -> from_list(Rest);
    Parts -> from_list(Parts)
  end.

-spec tail(Path :: t()) -> t().
tail(Path) -> filename:join(tl(filename:split(Path))).

-spec ensure(Path :: t()) -> ok.
ensure(Path) -> filelib:ensure_dir(Path).
