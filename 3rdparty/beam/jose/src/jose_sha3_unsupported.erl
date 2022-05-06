%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2016, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  11 Jan 2016 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(jose_sha3_unsupported).

-behaviour(jose_sha3).

%% jose_sha3 callbacks
-export([sha3_224/1]).
-export([sha3_256/1]).
-export([sha3_384/1]).
-export([sha3_512/1]).
-export([shake128/2]).
-export([shake256/2]).

%% Macros
-define(unsupported, erlang:error(sha3_unsupported)).

%%====================================================================
%% jose_sha3 callbacks
%%====================================================================

sha3_224(_InputBytes) ->
	?unsupported.

sha3_256(_InputBytes) ->
	?unsupported.

sha3_384(_InputBytes) ->
	?unsupported.

sha3_512(_InputBytes) ->
	?unsupported.

shake128(_InputBytes, _OutputByteLen) ->
	?unsupported.

shake256(_InputBytes, _OutputByteLen) ->
	?unsupported.
