-module(tempdir).

-export([new/0]).

-spec new() -> result:t(path:t(), term()).
new() ->
    TmpDir = path:from_list([
        <<"/tmp">>,
        io_lib:format(
            "_erlang_beam_~s_~s~s",
            [
                erlang:system_info(version),
                erlang:integer_to_list(erlang:system_time()),
                erlang:integer_to_list(erlang:monotonic_time())
            ]
        )
    ]),
    ok = path:ensure(TmpDir),
    {ok, TmpDir}.
