-module(cerl_analyzer_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(CWD(Path), filename:join(element(2, file:get_cwd()), Path)).

all() ->
    [
        fails_on_missing_parse_transform,
        handles_parse_transforms,
        handles_real_life_example_from_emqx,
        handles_real_life_example_from_emqx_with_parse_transforms,
        handles_real_life_example_from_verl,
        skips_dynamic_calls
    ].

skips_dynamic_calls(_Config) ->
    {ok, #{
        <<"../erlang/lifter/tests/fixtures/dynamic_calls.erl">> :=
            #{
                name := dynamic_calls,
                path := <<"../erlang/lifter/tests/fixtures/dynamic_calls.erl">>,
                external_calls := ExtCalls
            }
    }} = cerl_analyzer:analyze([<<"../erlang/lifter/tests/fixtures/dynamic_calls.erl">>]),

    ?assertMatch([#{calls := [], mfa := {dynamic_calls, foo, 1}}], ExtCalls).

handles_real_life_example_from_verl(_Config) ->
    % this change is to another test in the same file
    {ok, #{
        <<"../erlang/lifter/tests/fixtures/verl_SUITE.erl">> :=
            #{
                name := verl_SUITE,
                path := <<"../erlang/lifter/tests/fixtures/verl_SUITE.erl">>,
                exports := Exports,
                external_calls := ExtCalls,
                type_exports := TypeExports,
                includes := Includes
            }
    }} = cerl_analyzer:analyze([<<"../erlang/lifter/tests/fixtures/verl_SUITE.erl">>]),

    ?assertMatch(
        [
            {verl_SUITE, all, 0},
            {verl_SUITE, between_test, 1},
            {verl_SUITE, compare_test, 1},
            {verl_SUITE, compile_requirement_test, 1},
            {verl_SUITE, eq_test, 1},
            {verl_SUITE, gt_test, 1},
            {verl_SUITE, gte_test, 1},
            {verl_SUITE, is_match_test, 1},
            {verl_SUITE, lt_test, 1},
            {verl_SUITE, lte_test, 1},
            {verl_SUITE, parse_requirement_test, 1},
            {verl_SUITE, parse_test, 1}
        ],
        Exports
    ),

    ?assertMatch(
        [
            #{
                calls :=
                    [
                        {erlang, error, 1},
                        {erlang, 'not', 1},
                        {verl, lte, 2}
                    ],
                mfa := {verl_SUITE, lte_test, 1}
            },
            #{
                calls :=
                    [{erlang, error, 1}, {erlang, 'not', 1}, {verl, lt, 2}],
                mfa := {verl_SUITE, lt_test, 1}
            },
            #{
                calls :=
                    [
                        {erlang, error, 1},
                        {erlang, 'not', 1},
                        {verl, gte, 2}
                    ],
                mfa := {verl_SUITE, gte_test, 1}
            },
            #{
                calls :=
                    [
                        {verl, compile_requirement, 1},
                        {verl, is_match, 2},
                        {verl, is_match, 3},
                        {verl, parse, 1},
                        {verl, parse_requirement, 1}
                    ],
                mfa := {verl_SUITE, is_match_test, 1}
            },
            #{
                calls :=
                    [
                        {erlang, '<', 2},
                        {erlang, '>=', 2},
                        {erlang, is_binary, 1},
                        {erlang, is_reference, 1},
                        {erlang, system_info, 1},
                        {string, to_integer, 1},
                        {verl, compile_requirement, 1},
                        {verl, parse_requirement, 1}
                    ],
                mfa := {verl_SUITE, compile_requirement_test, 1}
            },
            #{
                calls :=
                    [
                        {erlang, '=:=', 2},
                        {erlang, 'and', 2},
                        {verl, parse_requirement, 1}
                    ],
                mfa := {verl_SUITE, parse_requirement_test, 1}
            },
            #{
                calls := [{erlang, '=:=', 2}, {verl, parse, 1}],
                mfa := {verl_SUITE, parse_test, 1}
            },
            #{
                calls :=
                    [{erlang, error, 1}, {erlang, 'not', 1}, {verl, gt, 2}],
                mfa := {verl_SUITE, gt_test, 1}
            },
            #{
                calls :=
                    [{erlang, error, 1}, {erlang, 'not', 1}, {verl, eq, 2}],
                mfa := {verl_SUITE, eq_test, 1}
            },
            #{
                calls := [{verl, compare, 2}],
                mfa := {verl_SUITE, compare_test, 1}
            },
            #{
                calls :=
                    [
                        {erlang, error, 1},
                        {erlang, 'not', 1},
                        {verl, between, 3}
                    ],
                mfa := {verl_SUITE, between_test, 1}
            },
            #{calls := [], mfa := {verl_SUITE, all, 0}}
        ],
        ExtCalls
    ),

    ?assertMatch([], TypeExports),

    ?assertMatch(
        [
            <<"../erlang/lifter/tests/fixtures/verl_SUITE.erl">>,
            <<"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-rules.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/common_test-1.23/include/ct.hrl">>,
            <<"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-rules.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/stdlib-4.0/include/assert.hrl">>
        ],
        Includes
    ).

handles_real_life_example_from_emqx(_Config) ->
    {ok, #{
        <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">> :=
            #{
                name := emqx_bpapi,
                path := <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">>,
                exports := Exports,
                external_calls := ExtCalls,
                type_exports := TypeExports,
                includes := Includes
            }
    }} = cerl_analyzer:analyze([
        <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">>,
        <<"../erlang/lifter/tests/fixtures/includes/emqx.hrl">>
    ]),

    ?assertMatch(
        [
            {emqx_bpapi, announce, 1},
            {emqx_bpapi, announce_fun, 1},
            {emqx_bpapi, behaviour_info, 1},
            {emqx_bpapi, start, 0},
            {emqx_bpapi, supported_version, 1},
            {emqx_bpapi, supported_version, 2},
            {emqx_bpapi, versions_file, 1}
        ],
        Exports
    ),

    ?assertMatch(
        [
            #{
                calls := [],
                mfa := {emqx_bpapi, behaviour_info, 1}
            },
            #{
                calls :=
                    [
                        {lists, min, 1},
                        {mnesia, select, 2},
                        {mnesia, write, 1}
                    ],
                mfa := {emqx_bpapi, update_minimum, 1}
            },
            #{
                calls :=
                    [
                        {erlang, error, 1},
                        {erlang, node, 0},
                        {mnesia, delete, 1},
                        {mnesia, select, 3},
                        {mnesia, write, 1}
                    ],
                mfa := {emqx_bpapi, announce_fun, 1}
            },
            #{
                calls :=
                    [{code, priv_dir, 1}, {filename, join, 2}],
                mfa := {emqx_bpapi, versions_file, 1}
            },
            #{
                calls :=
                    [
                        {emqx_bpapi, versions_file, 1},
                        {erlang, make_fun, 3},
                        {file, consult, 1},
                        {mria, transaction, 3}
                    ],
                mfa := {emqx_bpapi, announce, 1}
            },
            #{
                calls := [{ets, lookup_element, 3}],
                mfa := {emqx_bpapi, supported_version, 1}
            },
            #{
                calls := [{ets, lookup, 2}],
                mfa := {emqx_bpapi, supported_version, 2}
            },
            #{
                calls :=
                    [
                        {mria, create_table, 2},
                        {mria, wait_for_tables, 1}
                    ],
                mfa := {emqx_bpapi, start, 0}
            }
        ],
        ExtCalls
    ),

    ?assertMatch(
        [
            {emqx_bpapi, api, 0},
            {emqx_bpapi, api_version, 0},
            {emqx_bpapi, bpapi_meta, 0},
            {emqx_bpapi, call, 0},
            {emqx_bpapi, rpc, 0},
            {emqx_bpapi, var_name, 0}
        ],
        TypeExports
    ),

    ?assertMatch(
        [
            <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">>,
            <<"../erlang/lifter/tests/fixtures/includes/emqx.hrl">>,
            <<"../erlang/lifter/tests/fixtures/includes/emqx_bpapi.hrl">>,
            <<"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-rules.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/stdlib-4.0/include/ms_transform.hrl">>
        ],
        Includes
    ).

handles_real_life_example_from_emqx_with_parse_transforms(_Config) ->
    {ok, #{
        <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">> :=
            #{
                exports :=
                    [
                        {emqx_bpapi, announce, 1},
                        {emqx_bpapi, announce_fun, 1},
                        {emqx_bpapi, behaviour_info, 1},
                        {emqx_bpapi, start, 0},
                        {emqx_bpapi, supported_version, 1},
                        {emqx_bpapi, supported_version, 2},
                        {emqx_bpapi, versions_file, 1}
                    ],
                external_calls :=
                    [
                        #{
                            calls := [],
                            mfa := {emqx_bpapi, behaviour_info, 1}
                        },
                        #{
                            calls :=
                                [
                                    {lists, min, 1},
                                    {mnesia, select, 2},
                                    {mnesia, write, 1}
                                ],
                            mfa := {emqx_bpapi, update_minimum, 1}
                        },
                        #{
                            calls :=
                                [
                                    {erlang, error, 1},
                                    {erlang, node, 0},
                                    {mnesia, delete, 1},
                                    {mnesia, select, 3},
                                    {mnesia, write, 1}
                                ],
                            mfa := {emqx_bpapi, announce_fun, 1}
                        },
                        #{
                            calls :=
                                [{code, priv_dir, 1}, {filename, join, 2}],
                            mfa := {emqx_bpapi, versions_file, 1}
                        },
                        #{
                            calls :=
                                [
                                    {emqx_bpapi, versions_file, 1},
                                    {erlang, make_fun, 3},
                                    {file, consult, 1},
                                    {mria, transaction, 3}
                                ],
                            mfa := {emqx_bpapi, announce, 1}
                        },
                        #{
                            calls := [{ets, lookup_element, 3}],
                            mfa := {emqx_bpapi, supported_version, 1}
                        },
                        #{
                            calls := [{ets, lookup, 2}],
                            mfa := {emqx_bpapi, supported_version, 2}
                        },
                        #{
                            calls :=
                                [
                                    {mria, create_table, 2},
                                    {mria, wait_for_tables, 1}
                                ],
                            mfa := {emqx_bpapi, start, 0}
                        }
                    ],
                includes :=
                    [
                        <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">>,
                        <<"../erlang/lifter/tests/fixtures/emqx_bpapi.hrl">>,
                        <<"../erlang/lifter/tests/fixtures/includes/emqx.hrl">>,
                        <<"/warp/store/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-rules.warp.build/4a0758218cdd50e77098799caa7dfce67f56a69b88a273539e14470fb4af254d-erlang/otp_src_25.0/dist/lib/erlang/lib/stdlib-4.0/include/ms_transform.hrl">>
                    ],
                local_calls := [],
                name := emqx_bpapi,
                path :=
                    <<"../erlang/lifter/tests/fixtures/emqx_bpapi.erl">>,
                type_exports :=
                    [
                        {emqx_bpapi, api, 0},
                        {emqx_bpapi, api_version, 0},
                        {emqx_bpapi, bpapi_meta, 0},
                        {emqx_bpapi, call, 0},
                        {emqx_bpapi, rpc, 0},
                        {emqx_bpapi, var_name, 0}
                    ]
            },
        <<"../erlang/lifter/tests/fixtures/emqx_bpapi_trans.erl">> :=
            #{
                exports :=
                    [
                        {emqx_bpapi_trans, format_error, 1},
                        {emqx_bpapi_trans, parse_transform, 2}
                    ],
                external_calls :=
                    [
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, log, 2}
                        },
                        #{
                            calls :=
                                [
                                    {erlang, atom_to_list, 1},
                                    {erlang, list_to_atom, 1},
                                    {erlang, list_to_integer, 1},
                                    {re, run, 3}
                                ],
                            mfa :=
                                {emqx_bpapi_trans, api_and_version, 1}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, push_target, 2}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, push_err, 3}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, invalid_fun, 4}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, list_to_args, 1}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, call_or_cast, 1}
                        },
                        #{
                            calls :=
                                [{erlang, '=:=', 2}, {erlang, error, 1}],
                            mfa := {emqx_bpapi_trans, extract_mfa, 2}
                        },
                        #{
                            calls := [],
                            mfa :=
                                {emqx_bpapi_trans, extract_target_call, 2}
                        },
                        #{
                            calls := [{lists, map, 2}],
                            mfa :=
                                {emqx_bpapi_trans, extract_outer_args, 1}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, analyze_exprs, 6}
                        },
                        #{
                            calls := [{erlang, '=:=', 2}],
                            mfa := {emqx_bpapi_trans, analyze_fun, 5}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, is_attribute, 1}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, mk_export, 0}
                        },
                        #{
                            calls := [{erlang, error, 1}],
                            mfa := {emqx_bpapi_trans, mk_meta_fun, 1}
                        },
                        #{
                            calls :=
                                [
                                    {erlang, error, 1},
                                    {erlang, is_atom, 1},
                                    {erlang, is_float, 1},
                                    {erlang, is_integer, 1},
                                    {erlang, is_map, 1},
                                    {erlang, is_tuple, 1},
                                    {erlang, tuple_to_list, 1},
                                    {io_lib, deep_char_list, 1},
                                    {lists, flatten, 1},
                                    {maps, to_list, 1}
                                ],
                            mfa :=
                                {emqx_bpapi_trans, typerefl_quote__const, 2}
                        },
                        #{
                            calls :=
                                [{erlang, '++', 2}, {lists, splitwith, 2}],
                            mfa := {emqx_bpapi_trans, finalize, 2}
                        },
                        #{
                            calls := [],
                            mfa := {emqx_bpapi_trans, check, 1}
                        },
                        #{calls := [], mfa := {emqx_bpapi_trans, go, 2}},
                        #{
                            calls := [{erlang, error, 1}, {lists, foldl, 3}],
                            mfa :=
                                {emqx_bpapi_trans, parse_transform, 2}
                        },
                        #{
                            calls := [{io_lib, format, 2}],
                            mfa := {emqx_bpapi_trans, format_error, 1}
                        }
                    ],
                includes :=
                    [<<"../erlang/lifter/tests/fixtures/emqx_bpapi_trans.erl">>],
                local_calls := [],
                name := emqx_bpapi_trans,
                path :=
                    <<"../erlang/lifter/tests/fixtures/emqx_bpapi_trans.erl">>,
                type_exports := []
            },
        <<"../erlang/lifter/tests/fixtures/emqx_statsd_proto_v1.erl">> :=
            #{
                exports :=
                    [
                        {emqx_statsd_proto_v1, bpapi_meta, 0},
                        {emqx_statsd_proto_v1, introduced_in, 0},
                        {emqx_statsd_proto_v1, restart, 1},
                        {emqx_statsd_proto_v1, start, 1},
                        {emqx_statsd_proto_v1, stop, 1}
                    ],
                external_calls :=
                    [
                        #{
                            calls := [{rpc, multicall, 5}],
                            mfa := {emqx_statsd_proto_v1, restart, 1}
                        },
                        #{
                            calls := [{rpc, multicall, 5}],
                            mfa := {emqx_statsd_proto_v1, stop, 1}
                        },
                        #{
                            calls := [{rpc, multicall, 5}],
                            mfa := {emqx_statsd_proto_v1, start, 1}
                        },
                        #{
                            calls := [],
                            mfa :=
                                {emqx_statsd_proto_v1, introduced_in, 0}
                        },
                        #{
                            calls := [],
                            mfa :=
                                {emqx_statsd_proto_v1, bpapi_meta, 0}
                        }
                    ],
                includes :=
                    [
                        <<"../erlang/lifter/tests/fixtures/emqx_statsd_proto_v1.erl">>,
                        <<"../erlang/lifter/tests/fixtures/includes/bpapi.hrl">>
                    ],
                local_calls := [],
                name := emqx_statsd_proto_v1,
                path :=
                    <<"../erlang/lifter/tests/fixtures/emqx_statsd_proto_v1.erl">>,
                type_exports := []
            }
    }} =
        cerl_analyzer:analyze([
            <<"../erlang/lifter/tests/fixtures/includes/bpapi.hrl">>,
            <<"../erlang/lifter/tests/fixtures/emqx_bpapi_trans.erl">>,
            <<"../erlang/lifter/tests/fixtures/emqx_bpapi.hrl">>,
            <<"../erlang/lifter/tests/fixtures/includes/emqx.hrl">>,
            <<"../erlang/lifter/tests/fixtures/emqx_statsd_proto_v1.erl">>
        ]).

fails_on_missing_parse_transform(_Config) ->
    {ok, #{
        <<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">> :=
            #{
                error :=
                    #{
                        kind := missing_parse_transform,
                        path := <<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">>,
                        transform_name := dummy_transform
                    },
                path := <<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">>
            }
    }} =
        cerl_analyzer:analyze(
            [<<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">>]
        ).

handles_parse_transforms(_Config) ->
    {ok, #{
        <<"../erlang/lifter/tests/fixtures/dummy_transform.erl">> :=
            #{
                name := dummy_transform,
                path := <<"../erlang/lifter/tests/fixtures/dummy_transform.erl">>
            }
    }} = cerl_analyzer:analyze([<<"../erlang/lifter/tests/fixtures/dummy_transform.erl">>]),

    {ok, #{
        <<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">> :=
            #{
                name := needs_parse_transform,
                path := <<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">>,
                exports := Exports,
                external_calls := ExtCalls,
                type_exports := TypeExports,
                includes := Includes
            }
    }} = cerl_analyzer:analyze([
        <<"../erlang/lifter/tests/fixtures/dummy_transform.erl">>,
        <<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">>
    ]),

    ?assertMatch([], Exports),

    ?assertMatch([], ExtCalls),

    ?assertMatch([], TypeExports),

    ?assertMatch([<<"../erlang/lifter/tests/fixtures/needs_parse_transform.erl">>], Includes).