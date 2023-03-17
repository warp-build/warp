defmodule Tricorder.Analysis.ErlangTest do
  use ExUnit.Case

  test "detects missing includes" do
    assert {:ok, {:missing_dependencies, %{includes: ["missing_header.hrl"]}}} =
             Tricorder.Analysis.Erlang.analyze("./test/fixtures/missing_includes.erl", %{
               include_paths: [],
               code_paths: []
             })
  end

  test "detects needing parse transforms" do
    assert {:ok, {:missing_dependencies, %{modules: [:missing_transform]}}} =
             Tricorder.Analysis.Erlang.analyze("./test/fixtures/missing_parse_transforms.erl", %{
               include_paths: [],
               code_paths: []
             })
  end

  test "detects direct module depedencies" do
    assert {:ok,
            {:completed,
             [
               %{
                 includes: [],
                 modules: [:dep_c, :dep_b, :dep_a],
                 name: "./test/fixtures/direct_deps.erl",
                 rule: "erlang_library",
                 srcs: ["direct_deps.erl"]
               }
             ]}} =
             Tricorder.Analysis.Erlang.analyze("./test/fixtures/direct_deps.erl", %{
               include_paths: [],
               code_paths: []
             })
  end

  test "detects direct type depedencies" do
    assert {
             :ok,
             {
               :completed,
               [
                 %{
                   includes: [],
                   modules: [:panic, :option, :result],
                   name: "./test/fixtures/direct_type_deps.erl",
                   rule: "erlang_library",
                   srcs: ["direct_type_deps.erl"]
                 }
               ]
             }
           } =
             Tricorder.Analysis.Erlang.analyze("./test/fixtures/direct_type_deps.erl", %{
               include_paths: [],
               code_paths: []
             })
  end

  test "detects imported depedencies" do
    assert {
             :ok,
             {
               :completed,
               [
                 %{
                   includes: [],
                   modules: [:dep_c, :dep_b, :dep_a],
                   name: "./test/fixtures/imported_deps.erl",
                   rule: "erlang_library",
                   srcs: ["imported_deps.erl"]
                 }
               ]
             }
           } =
             Tricorder.Analysis.Erlang.analyze("./test/fixtures/imported_deps.erl", %{
               include_paths: [],
               code_paths: []
             })
  end

  test "detects includes" do
    assert {
             :ok,
             {
               :completed,
               [
                 %{
                   includes: ["./test/fixtures/found_header.hrl"],
                   modules: [],
                   name: "./test/fixtures/found_includes.erl",
                   rule: "erlang_library",
                   srcs: ["found_includes.erl"]
                 }
               ]
             }
           } =
             Tricorder.Analysis.Erlang.analyze("./test/fixtures/found_includes.erl", %{
               include_paths: [],
               code_paths: []
             })
  end
end
