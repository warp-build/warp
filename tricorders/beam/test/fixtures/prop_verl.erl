-module(prop_verl).

-include_lib("proper.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

% test for equality with opaque term
-dialyzer({no_opaque, prop_basic_valid_semver0/0}).
prop_basic_valid_semver0() ->
    ?FORALL(
        {Maj, Min, P, Pre},
        {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_empty(binary())},
        begin
            Major = integer_to_binary(Maj),
            Minor = integer_to_binary(Min),
            Patch = integer_to_binary(P),
            V =
                <<Major/binary, <<".">>/binary, Minor/binary, <<".">>/binary, Patch/binary,
                    <<"-">>/binary, Pre/binary>>,
            case {re:run(Pre, "^[0-9A-Za-z-+]+$"), re:run(Pre, "(^0[0-9]+)|(^[+]$)|[\r\n]")} of
                {nomatch, _} ->
                    {error, invalid_version} =:= verl:parse(V);
                {_, {match, _}} ->
                    {error, invalid_version} =:= verl:parse(V);
                _ ->
                    {ok, Parsed} = verl:parse(V),
                    PreEl =
                        case re:run(Pre, "^[0-9]+$") of
                            nomatch ->
                                [Pre];
                            _ ->
                                [binary_to_integer(Pre)]
                        end,
                    Exp = #{
                        build => undefined,
                        major => Maj,
                        minor => Min,
                        patch => P,
                        pre => PreEl
                    },
                    Exp =:= Parsed
            end
        end
    ).

% test for equality with opaque term
-dialyzer({no_opaque, prop_basic_valid_semver/0}).
prop_basic_valid_semver() ->
    ?FORALL(
        {Maj, Min, P},
        {non_neg_integer(), non_neg_integer(), non_neg_integer()},
        begin
            Major = integer_to_binary(Maj),
            Minor = integer_to_binary(Min),
            Patch = integer_to_binary(P),
            V = <<Major/binary, <<".">>/binary, Minor/binary, <<".">>/binary, Patch/binary>>,
            {ok, Parsed} = verl:parse(V),
            Exp = #{build => undefined, major => Maj, minor => Min, patch => P, pre => []},
            Exp =:= Parsed
        end
    ).

prop_basic_invalid_semver() ->
    ?FORALL(
        {Maj, Min, P},
        {neg_integer(), neg_integer(), neg_integer()},
        begin
            Major = integer_to_binary(Maj),
            Minor = integer_to_binary(Min),
            Patch = integer_to_binary(P),
            V = <<Major/binary, <<".">>/binary, Minor/binary, <<".">>/binary, Patch/binary>>,
            {error, invalid_version} =:= verl:parse(V)
        end
    ).

prop_basic_invalid_semver_more() ->
    ?FORALL(
        {Maj, Min, P},
        {any(), any(), any()},
        begin
            Major = term_to_binary(Maj),
            Minor = term_to_binary(Min),
            Patch = term_to_binary(P),
            V = <<Major/binary, <<".">>/binary, Minor/binary, <<".">>/binary, Patch/binary>>,
            {error, invalid_version} =:= verl:parse(V)
        end
    ).

prop_basic_invalid_semver_more2() ->
    ?FORALL(
        {Maj, Min, P, Pre},
        {binary(), binary(), binary(), binary()},
        begin
            V =
                <<Maj/binary, <<".">>/binary, Min/binary, <<".">>/binary, P/binary, <<"-">>/binary,
                    Pre/binary>>,
            {error, invalid_version} =:= verl:parse(V)
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
