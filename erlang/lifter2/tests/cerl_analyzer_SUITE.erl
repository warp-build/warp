-module(cerl_analyzer_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(CWD(Path), filename:join(element(2, file:get_cwd()), Path)).

all() ->
    [
        handles_real_life_example_from_verl,
        handles_real_life_example_from_emqx
    ].


handles_real_life_example_from_verl(_Config) ->
	{ok, #{
				 name := verl_SUITE,
				 path := <<"../erlang/lifter2/tests/fixtures/verl_SUITE.erl">>,
				 exports := Exports,
				 external_calls := ExtCalls,
				 type_exports := TypeExports,
				 includes := Includes
				}} = cerl_analyzer:analyze("../erlang/lifter2/tests/fixtures/verl_SUITE.erl"),

	?assertMatch([#{a := 0,f := all,m := verl_SUITE},
								#{a := 1,f := between_test, m := verl_SUITE},
								#{a := 1,f := compare_test, m := verl_SUITE},
								#{a := 1, f := compile_requirement_test, m := verl_SUITE},
								#{a := 1,f := eq_test, m := verl_SUITE},
								#{a := 1,f := gt_test, m := verl_SUITE},
								#{a := 1,f := gte_test, m := verl_SUITE},
								#{a := 1,f := is_match_test, m := verl_SUITE},
								#{a := 1,f := lt_test, m := verl_SUITE},
								#{a := 1,f := lte_test, m := verl_SUITE},
								#{a := 1, f := parse_requirement_test, m := verl_SUITE},
								#{a := 1,f := parse_test, m := verl_SUITE}] , Exports),

	?assertMatch(
		 [#{calls :=
				[#{a := 1,f := error,m := erlang},
				 #{a := 1,f := 'not',m := erlang},
				 #{a := 2,f := lte,m := verl}],
				mfa :=
				#{a := 1,f := lte_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := error,m := erlang},
				 #{a := 1,f := 'not',m := erlang},
				 #{a := 2,f := lt,m := verl}],
				mfa :=
				#{a := 1,f := lt_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := error,m := erlang},
				 #{a := 1,f := 'not',m := erlang},
				 #{a := 2,f := gte,m := verl}],
				mfa :=
				#{a := 1,f := gte_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := compile_requirement,
					 m := verl},
				 #{a := 1,f := parse,m := verl},
				 #{a := 1,f := parse_requirement,
					 m := verl},
				 #{a := 2,f := is_match,m := verl},
				 #{a := 3,f := is_match,m := verl}],
				mfa :=
				#{a := 1,f := is_match_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := compile_requirement,
					 m := verl},
				 #{a := 1,f := is_binary,m := erlang},
				 #{a := 1,f := is_reference,
					 m := erlang},
				 #{a := 1,f := parse_requirement,
					 m := verl},
				 #{a := 1,f := system_info,
					 m := erlang},
				 #{a := 1,f := to_integer,m := string},
				 #{a := 2,f := '<',m := erlang},
				 #{a := 2,f := '>=',m := erlang}],
				mfa :=
				#{a := 1,
					f := compile_requirement_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := parse_requirement,
					 m := verl},
				 #{a := 2,f := '=:=',m := erlang},
				 #{a := 2,f := 'and',m := erlang}],
				mfa :=
				#{a := 1,f := parse_requirement_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := parse,m := verl},
				 #{a := 2,f := '=:=',m := erlang}],
				mfa :=
				#{a := 1,f := parse_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := error,m := erlang},
				 #{a := 1,f := 'not',m := erlang},
				 #{a := 2,f := gt,m := verl}],
				mfa :=
				#{a := 1,f := gt_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := error,m := erlang},
				 #{a := 1,f := 'not',m := erlang},
				 #{a := 2,f := eq,m := verl}],
				mfa :=
				#{a := 1,f := eq_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 2,f := compare,m := verl}],
				mfa :=
				#{a := 1,f := compare_test,
					m := verl_SUITE}},
			#{calls :=
				[#{a := 1,f := error,m := erlang},
				 #{a := 1,f := 'not',m := erlang},
				 #{a := 3,f := between,m := verl}],
				mfa :=
				#{a := 1,f := between_test,
					m := verl_SUITE}},
			#{calls := [],
				mfa :=
				#{a := 0,f := all,
					m := verl_SUITE}}]
		 , ExtCalls),

  ?assertMatch([], TypeExports),

	?assertMatch(
		 ["../erlang/lifter2/tests/fixtures/verl_SUITE.erl",
			"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/common_test-1.23/include/ct.hrl",
			"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/stdlib-4.0/include/assert.hrl"]
		 , Includes).

handles_real_life_example_from_emqx(_Config) ->
	{ok, #{
				 name := emqx_bpapi,
				 path := <<"../erlang/lifter2/tests/fixtures/emqx_bpapi.erl">>,
				 exports := Exports,
				 external_calls := ExtCalls,
				 type_exports := TypeExports,
				 includes := Includes
				}} = cerl_analyzer:analyze("../erlang/lifter2/tests/fixtures/emqx_bpapi.erl"),

	?assertMatch([#{a := 0,f := start,m := emqx_bpapi},
								#{a := 1,f := announce,m := emqx_bpapi},
								#{a := 1,f := announce_fun,m := emqx_bpapi},
								#{a := 1,f := behaviour_info,
									m := emqx_bpapi},
								#{a := 1,f := supported_version,
									m := emqx_bpapi},
								#{a := 1,f := versions_file,m := emqx_bpapi},
								#{a := 2,f := supported_version,
									m := emqx_bpapi}], Exports),

	?assertMatch(
		 [#{calls := [],
				mfa := #{a := 1,f := behaviour_info, m := emqx_bpapi}},
			#{calls := [#{a := 1,f := min,m := lists},
									#{a := 1,f := write,m := mnesia},
									#{a := 2,f := select,m := mnesia}],
				mfa := #{a := 1,f := update_minimum, m := emqx_bpapi}},
			#{calls := [#{a := 0,f := node,m := erlang},
									#{a := 1,f := delete,m := mnesia},
									#{a := 1,f := error,m := erlang},
									#{a := 1,f := write,m := mnesia},
									#{a := 3,f := select,m := mnesia}],
				mfa := #{a := 1,f := announce_fun, m := emqx_bpapi}},
			#{calls := [#{a := 1,f := priv_dir,m := code},
									#{a := 2,f := join,m := filename}],
				mfa := #{a := 1,f := versions_file, m := emqx_bpapi}},
			#{calls := [#{a := 1,f := consult,m := file},
									#{a := 1,f := versions_file, m := emqx_bpapi},
									#{a := 3,f := make_fun,m := erlang},
									#{a := 3,f := transaction,m := mria}],
				mfa := #{a := 1,f := announce, m := emqx_bpapi}},
			#{calls := [#{a := 3,f := lookup_element, m := ets}],
				mfa := #{a := 1,f := supported_version, m := emqx_bpapi}},
			#{calls := [#{a := 2,f := lookup,m := ets}],
				mfa := #{a := 2,f := supported_version, m := emqx_bpapi}},
			#{calls := [#{a := 1,f := wait_for_tables, m := mria},
									#{a := 2,f := create_table, m := mria}],
				mfa := #{a := 0,f := start, m := emqx_bpapi}}]
		 , ExtCalls),

  ?assertMatch(
		 [#{a := 0,f := api,m := emqx_bpapi},
			#{a := 0,f := api_version,m := emqx_bpapi},
			#{a := 0,f := bpapi_meta,m := emqx_bpapi},
			#{a := 0,f := call,m := emqx_bpapi},
			#{a := 0,f := rpc,m := emqx_bpapi},
			#{a := 0,f := var_name,m := emqx_bpapi}]
		 , TypeExports),

	?assertMatch(
		 ["../erlang/lifter2/tests/fixtures/emqx.hrl",
			"../erlang/lifter2/tests/fixtures/emqx_bpapi.erl",
			"../erlang/lifter2/tests/fixtures/emqx_bpapi.hrl",
			"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/stdlib-4.0/include/ms_transform.hrl"]
		 , Includes).
