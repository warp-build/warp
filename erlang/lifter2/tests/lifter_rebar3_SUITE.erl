-module(lifter_rebar3_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(CWD(Path), filename:join(element(2, file:get_cwd()), Path)).

all() ->
  [
   returns_an_empty_list_for_no_projects,
   finds_projects
  ].


returns_an_empty_list_for_no_projects(_Config) ->
  {ok, #{}} = lifter_rebar3:find_all_rebar_projects(<<"../erlang/lifter2/tests/fixtures/bad_path">>).

finds_projects(_Config) ->
	{ok,
	 #{<<"../erlang/lifter2/tests/fixtures/rebar.config">> :=
		 #{deps :=
			 #{cowboy :=
				 {git,"https://github.com/emqx/cowboy",
					{tag,"2.9.0"}},
				 ekka :=
				 {git,"https://github.com/emqx/ekka",
					{tag,"0.13.4"}},
				 esockd :=
				 {git,"https://github.com/emqx/esockd",
					{tag,"5.9.4"}},
				 gen_rpc :=
				 {git,"https://github.com/emqx/gen_rpc",
					{tag,"2.8.1"}},
				 gproc :=
				 {git,"https://github.com/uwiger/gproc",
					{tag,"0.8.0"}},
				 hocon :=
				 {git,"https://github.com/emqx/hocon.git",
					{tag,"0.30.0"}},
				 jiffy :=
				 {git,"https://github.com/emqx/jiffy",
					{tag,"1.0.5"}},
				 lc :=
				 {git,"https://github.com/emqx/lc.git",
					{tag,"0.3.1"}},
				 pbkdf2 :=
				 {git,
					"https://github.com/emqx/erlang-pbkdf2.git",
					{tag,"2.0.4"}},
				 recon :=
				 {git,"https://github.com/ferd/recon",
					{tag,"2.5.1"}},
				 snabbkaffe :=
				 {git,
					"https://github.com/kafka4beam/snabbkaffe.git",
					{tag,"1.0.0"}}},
			 dialyzer :=
			 #{plt_apps := all_apps,
				 plt_extra_apps := [hocon],
				 plt_location := <<".">>,
				 plt_prefix := <<"emqx_dialyzer">>,
				 statistics := true,
				 warnings :=
				 [unmatched_returns,error_handling,
					race_conditions]},
			 erl_opts :=
			 [warn_unused_vars,warn_shadow_vars,
				warn_unused_import,warn_obsolete_guard,
				compressed],
			 extra_src_dirs := #{<<"etc">> := [recursive]},
			 name := <<"fixtures">>,
			 plugins := #{rebar3_proper := <<"0.12.1">>},
			 profiles :=
			 #{test :=
				 #{deps :=
					 #{bbmustache := <<"1.10.0">>,
						 emqtt :=
						 {git,
							"https://github.com/emqx/emqtt",
							{tag,"1.6.0"}},
						 meck := <<"0.9.2">>,
						 proper := <<"1.4.0">>},
					 extra_src_dirs :=
					 #{<<"test">> := [recursive]}}},
			 project_plugins := [erlfmt],
			 root := <<"../erlang/lifter2/tests/fixtures">>,
			 srcs :=
			 #{includes := [],priv := [],srcs := [],
				 tests := []},
			 xref_checks :=
			 [undefined_function_calls,undefined_functions,
				locals_not_used,deprecated_function_calls,
				warnings_as_errors,deprecated_functions]}}}
	= lifter_rebar3:find_all_rebar_projects(<<"../erlang/lifter2/tests/fixtures">>).
