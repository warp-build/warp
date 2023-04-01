defmodule Tricorder.Analysis.CommonTestTest do
  use ExUnit.Case

  alias Tricorder.Analysis.Erlang.CommonTest

  test "use-case: returns all test cases without config" do
    assert [
             %{
               cases: ["a1_test"],
               includes: [],
               modules: [],
               name: "a1_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_without_groups_SUITE.erl"
             },
             %{
               cases: ["a2_test"],
               includes: [],
               modules: [],
               name: "a2_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_without_groups_SUITE.erl"
             },
             %{
               cases: ["b_test"],
               includes: [],
               modules: [],
               name: "b_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_without_groups_SUITE.erl"
             },
             %{
               cases: ["c_test"],
               includes: [],
               modules: [],
               name: "c_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_without_groups_SUITE.erl"
             }
           ] =
             CommonTest.suites(
               "./test/fixtures/ct_suite_without_groups_SUITE.erl",
               _test_matcher = :all,
               _modules = [],
               _includes = [],
               _paths = %{
                 include_paths: [],
                 code_paths: []
               }
             )
             |> Enum.sort()
  end

  test "use-case: returns matching test cases without config" do
    assert [
             %{
               cases: ["a1_test"],
               includes: [],
               modules: [],
               name: "a1_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_without_groups_SUITE.erl"
             },
             %{
               cases: ["a2_test"],
               includes: [],
               modules: [],
               name: "a2_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_without_groups_SUITE.erl"
             }
           ] =
             CommonTest.suites(
               "./test/fixtures/ct_suite_without_groups_SUITE.erl",
               _test_matcher = {:match, "a"},
               _modules = [],
               _includes = [],
               _paths = %{
                 include_paths: [],
                 code_paths: []
               }
             )
             |> Enum.sort()
  end

  test "use-case: returns all test cases with config" do
    assert [
             %{
               cases: ["a_test"],
               includes: [],
               modules: [],
               name: "group_a:a_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_with_groups_SUITE.erl",
               groups: ["group_a"],
               group_opts: [],
               test_opts: []
             },
             %{
               cases: ["b_test"],
               includes: [],
               modules: [],
               name: "group_a:b_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_with_groups_SUITE.erl",
               groups: ["group_a"],
               group_opts: [],
               test_opts: []
             },
             %{
               cases: ["c_test"],
               includes: [],
               modules: [],
               name: "group_b:c_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_with_groups_SUITE.erl",
               groups: ["group_b"],
               group_opts: [:parallel],
               test_opts: []
             }
           ] =
             CommonTest.suites(
               "./test/fixtures/ct_suite_with_groups_SUITE.erl",
               _test_matcher = :all,
               _modules = [],
               _includes = [],
               _paths = %{
                 include_paths: [],
                 code_paths: []
               }
             )
             |> Enum.sort()
  end
end
