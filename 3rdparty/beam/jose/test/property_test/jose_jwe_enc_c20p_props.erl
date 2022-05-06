%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(jose_jwe_enc_c20p_props).

-include_lib("proper/include/proper.hrl").

% -compile(export_all).

base64url_binary() ->
	?LET(Binary,
		binary(),
		jose_jwa_base64url:encode(Binary)).

binary_map() ->
	?LET(List,
		list({base64url_binary(), base64url_binary()}),
		maps:from_list(List)).

enc() ->
	return(<<"C20P">>).

jwk_jwe_maps() ->
	?LET(ENC,
		enc(),
		begin
			JWEMap = #{
				<<"alg">> => <<"dir">>,
				<<"enc">> => ENC
			},
			Key = element(2, jose_jwk:to_oct(jose_jwe:generate_key(JWEMap))),
			JWKMap = #{
				<<"kty">> => <<"oct">>,
				<<"k">> => jose_jwa_base64url:encode(Key)
			},
			{Key, JWKMap, JWEMap}
		end).

jwk_jwe_gen() ->
	?LET({Key, JWKMap, JWEMap},
		jwk_jwe_maps(),
		{Key, jose_jwk:from_map(JWKMap), jose_jwe:from_map(JWEMap)}).

prop_from_map_and_to_map() ->
	?FORALL(JWEMap,
		?LET({{_Key, _JWKMap, JWEMap}, Extras},
			{jwk_jwe_maps(), binary_map()},
			maps:merge(Extras, JWEMap)),
		begin
			JWE = jose_jwe:from_map(JWEMap),
			JWEMap =:= element(2, jose_jwe:to_map(JWE))
		end).

prop_block_encrypt_and_block_decrypt() ->
	?FORALL({CEK, IV, JWK, JWE, PlainText},
		?LET({CEK, JWK, JWE},
			jwk_jwe_gen(),
			{CEK, jose_jwe:next_iv(JWE), JWK, JWE, binary()}),
		begin
			Encrypted = jose_jwe:block_encrypt(JWK, PlainText, CEK, IV, JWE),
			CompactEncrypted = jose_jwe:compact(Encrypted),
			PlainText =:= element(1, jose_jwe:block_decrypt(JWK, CompactEncrypted))
		end).
