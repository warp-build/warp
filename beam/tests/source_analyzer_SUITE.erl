-module(source_analyzer_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(CWD(Path), path:join(element(2, file:get_cwd()), Path)).

all() ->
  [
   handles_real_life_example_from_emqx
  ].

handles_real_life_example_from_emqx(_Config) ->
	{ok,#{<<"../erlang/lifter/tests/fixtures/emqx_authn_mysql.erl.wsig">> :=
				[#{deps :=
					 [<<"//.:emqx_authn.hrl">>,
						<<"//emqx/include:logger.hrl">>,
						<<"https://hex.pm/packages/hocon">>
					 ],
					 name := <<"emqx_authn_mysql.erl">>,
					 rule := <<"erlang_library">>,
					 runtime_deps :=
					 [<<"emqx_authn_password_hashing">>,
						<<"emqx_authn_utils">>,
						<<"emqx_resource">>],
					 srcs :=
					 [<<"emqx_authn_mysql.erl">>]}]}}
  = source_analyzer:analyze(
                   _Root = ?CWD("."),
                   _Files = [ <<"../erlang/lifter/tests/fixtures/emqx_authn_mysql.erl">> ],
                   _ModMap = #{ 
                      hocon => <<"https://hex.pm/packages/hocon">>,
											<<"hocon/include/hoconsc.hrl">> => <<"https://hex.pm/packages/hocon">>
                    },
                   _IgnoreModMap = #{},
                   _IncludePaths = [?CWD("fixtures"), ?CWD("fixtures/includes")]
                  ).
