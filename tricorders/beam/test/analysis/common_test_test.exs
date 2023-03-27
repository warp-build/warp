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
               :all,
               [],
               [],
               %{
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
               {:match, "a"},
               [],
               [],
               %{
                 include_paths: [],
                 code_paths: []
               }
             )
             |> Enum.sort()
  end

  test "use-case: returns all test cases with config" do
    assert [
             %{
               cases: ["group_a:a_test"],
               includes: [],
               modules: [],
               name: "group_a:a_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_with_groups_SUITE.erl"
             },
             %{
               cases: ["group_a:b_test"],
               includes: [],
               modules: [],
               name: "group_a:b_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_with_groups_SUITE.erl"
             },
             %{
               cases: ["group_b:c_test"],
               includes: [],
               modules: [],
               name: "group_b:c_test",
               rule: "erlang_test",
               test: "./test/fixtures/ct_suite_with_groups_SUITE.erl"
             }
           ] =
             CommonTest.suites(
               "./test/fixtures/ct_suite_with_groups_SUITE.erl",
               :all,
               [],
               [],
               %{
                 include_paths: [],
                 code_paths: []
               }
             )
             |> Enum.sort()
  end
end
