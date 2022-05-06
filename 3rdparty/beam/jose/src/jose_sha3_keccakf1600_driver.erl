%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2016, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  20 Jan 2016 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(jose_sha3_keccakf1600_driver).

-behaviour(jose_sha3).

%% jose_sha3 callbacks
-export([sha3_224/1]).
-export([sha3_256/1]).
-export([sha3_384/1]).
-export([sha3_512/1]).
-export([shake128/2]).
-export([shake256/2]).

%%====================================================================
%% jose_sha3 callbacks
%%====================================================================

sha3_224(InputBytes) ->
	keccakf1600_fips202:sha3_224(InputBytes).

sha3_256(InputBytes) ->
	keccakf1600_fips202:sha3_256(InputBytes).

sha3_384(InputBytes) ->
	keccakf1600_fips202:sha3_384(InputBytes).

sha3_512(InputBytes) ->
	keccakf1600_fips202:sha3_512(InputBytes).

shake128(InputBytes, OutputByteLen) ->
	keccakf1600_fips202:shake128(InputBytes, OutputByteLen).

shake256(InputBytes, OutputByteLen) ->
	keccakf1600_fips202:shake256(InputBytes, OutputByteLen).
