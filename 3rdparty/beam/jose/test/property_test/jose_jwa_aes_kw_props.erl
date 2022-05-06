%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(jose_jwa_aes_kw_props).

-include_lib("proper/include/proper.hrl").

% -compile(export_all).

kek_size() -> oneof([128, 192, 256]).

key_wrapper_gen() ->
	?LET({Bits, PlainText},
		{kek_size(), binary()},
		{Bits, binary(Bits div 8), jose_jwa_pkcs7:pad(PlainText)}).

prop_wrap_and_unwrap() ->
	?FORALL({_Bits, KEK, PlainText},
		key_wrapper_gen(),
		begin
			CipherText = jose_jwa_aes_kw:wrap(PlainText, KEK),
			PlainText =:= jose_jwa_aes_kw:unwrap(CipherText, KEK)
		end).
