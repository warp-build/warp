-module(path).

-export([new/1]).
-export([basename/1]).
-export([extension/1]).
-export([contains/2]).
-export([from_list/1]).
-export([join/2]).
-export([add_extension/2]).
-export([relativize/1]).
-export([strip_prefix/2]).
-export([tail/1]).
-export([to_list/1]).
-export([to_string/1]).

-export_type([t/0]).

-opaque t() :: binary().

new(Str) -> binary:list_to_bin([Str]).

-spec strip_prefix(Prefix :: t(), Path :: t()) -> t().
strip_prefix(Prefix, Path) -> from_list([ "." | string:replace(Path, Prefix, "") ]).

join(A, B) -> from_list([A, B]).

extension(Path) -> new(filename:extension(Path)).

add_extension(Path, Ext) -> new([Path, ".", Ext]).

basename(Path) -> new(filename:basename(Path)).

contains(Path, Str) -> not (string:find(Path, Str) == nomatch).

to_list(Path) -> [ new(Seg) || Seg <- filename:split(Path) ].

to_string(Path) -> binary:bin_to_list(Path).

from_list([]) -> new("");
from_list(Ls) when is_list(Ls) -> new(filename:join(Ls)).

relativize(Path) ->
  case to_list(Path) of
    [<<".">> | Rest] -> from_list(Rest);
    Parts -> from_list(Parts)
  end.

-spec tail(Path :: t()) -> t().
tail(Path) -> filename:join(tl(filename:split(Path))).

