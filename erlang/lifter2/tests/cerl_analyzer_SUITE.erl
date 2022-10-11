-module(cerl_analyzer_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(CWD(Path), filename:join(element(2, file:get_cwd()), Path)).

all() ->
    [
        handles_real_life_example
    ].


handles_real_life_example(_Config) ->
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

  ?assertMatch([], Includes).
