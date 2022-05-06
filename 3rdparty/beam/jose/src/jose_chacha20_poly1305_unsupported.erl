%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2016, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2016 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(jose_chacha20_poly1305_unsupported).

-behaviour(jose_chacha20_poly1305).

%% jose_chacha20_poly1305 callbacks
-export([decrypt/5]).
-export([encrypt/4]).
-export([authenticate/3]).
-export([verify/4]).

%% Macros
-define(unsupported, erlang:error(chacha20_poly1305_unsupported)).

%%====================================================================
%% jose_chacha20_poly1305 callbacks
%%====================================================================

decrypt(_CipherText, _CipherTag, _AAD, _IV, _Key) ->
	?unsupported.

encrypt(_PlainText, _AAD, _IV, _Key) ->
	?unsupported.

authenticate(_Message, _Key, _Nonce) ->
	?unsupported.

verify(_MAC, _Message, _Key, _Nonce) ->
	?unsupported.
