%% Copyright (c) 2019-2020, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(gun_raw).

-export([check_options/1]).
-export([name/0]).
-export([opts_name/0]).
-export([has_keepalive/0]).
-export([init/4]).
-export([handle/5]).
-export([closing/4]).
-export([close/4]).
-export([data/7]).
%% @todo down

-record(raw_state, {
	ref :: undefined | gun:stream_ref(),
	reply_to :: pid(),
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module()
}).

%% @todo Reject ALL options.
check_options(_) ->
	ok.

name() -> raw.
opts_name() -> raw_opts.
has_keepalive() -> false.

init(ReplyTo, Socket, Transport, Opts) ->
	StreamRef = maps:get(stream_ref, Opts, undefined),
	{connected_data_only, #raw_state{ref=StreamRef, reply_to=ReplyTo, socket=Socket, transport=Transport}}.

handle(Data, State=#raw_state{ref=StreamRef, reply_to=ReplyTo}, CookieStore, _, EvHandlerState) ->
	%% When we take over the entire connection there is no stream reference.
	ReplyTo ! {gun_data, self(), StreamRef, nofin, Data},
	{{state, State}, CookieStore, EvHandlerState}.

%% We can always close immediately.
closing(_, _, _, EvHandlerState) ->
	{close, EvHandlerState}.

close(_, _, _, EvHandlerState) ->
	EvHandlerState.

%% @todo Initiate closing on IsFin=fin.
data(State=#raw_state{ref=StreamRef, socket=Socket, transport=Transport}, StreamRef,
		_ReplyTo, _IsFin, Data, _EvHandler, EvHandlerState) ->
	Transport:send(Socket, Data),
	{State, EvHandlerState}.
