-module(result).

-export_type([t/1]).
-export_type([t/2]).

-opaque t(Err) :: ok | {err, Err}.
-opaque t(Ok, Err) :: {ok, Ok} | {err, Err}.
