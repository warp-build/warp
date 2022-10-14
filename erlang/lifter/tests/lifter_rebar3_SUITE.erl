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
  {ok, #{}} = lifter_rebar3:find_all_rebar_projects(<<"../erlang/lifter/tests/fixtures/bad_path">>).

finds_projects(_Config) ->
  {ok,
   #{<<"../erlang/lifter/tests/fixtures/rebar.config">> :=
     #{deps :=
       #{cowboy :=
         {git,"https://github.com/emqx/cowboy",
          {tag,"2.9.0"}}},
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
         #{deps := #{proper := <<"1.4.0">>},
           extra_src_dirs :=
           #{<<"test">> := [recursive]}}},
       project_plugins := [erlfmt],
       root := <<"../erlang/lifter/tests/fixtures">>,
       srcs :=
       #{includes := [],priv := [],srcs := [],
         tests := []},
       xref_checks :=
       [undefined_function_calls,undefined_functions,
        locals_not_used,deprecated_function_calls,
        warnings_as_errors,deprecated_functions]}}}
  = lifter_rebar3:find_all_rebar_projects(<<"../erlang/lifter/tests/fixtures">>).
