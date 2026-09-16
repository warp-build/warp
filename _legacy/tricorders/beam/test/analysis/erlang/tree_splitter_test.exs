defmodule Tricorder.Analysis.Erlang.TreeSplitterTest do
  use ExUnit.Case

  alias Tricorder.Analysis.Erlang.TreeSplitter
  alias Tricorder.Analysis.TestMatcher

  @default_paths %{include_paths: [], code_paths: []}

  test "on matching @all all the test subtrees" do
    assert {:ok,
            [
              %{
                ast: [
                  {:attribute, 0, :file,
                   {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                  {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                  {:attribute, 0, :export, [foo_3_test: 1]},
                  {:function, 0, :foo_3_test, 1,
                   [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :baz}]}]},
                  {:eof, 0}
                ],
                name: {:foo_3_test, 1}
              },
              %{
                ast: [
                  {:attribute, 0, :file,
                   {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                  {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                  {:attribute, 0, :export, [foo_2_test: 1]},
                  {:function, 0, :foo_2_test, 1,
                   [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :baz}]}]},
                  {:eof, 0}
                ],
                name: {:foo_2_test, 1}
              },
              %{
                ast: [
                  {:attribute, 0, :file,
                   {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                  {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                  {:attribute, 0, :export, [foo_1_test: 1]},
                  {:function, 0, :foo_1_test, 1,
                   [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :bar}]}]},
                  {:eof, 0}
                ],
                name: {:foo_1_test, 1}
              },
              %{
                ast: [
                  {:attribute, 0, :file,
                   {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                  {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                  {:attribute, 0, :export, [prop_a: 1]},
                  {:function, 0, :prop_a, 1,
                   [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}]},
                  {:eof, 0}
                ],
                name: {:prop_a, 1}
              }
            ]} =
             TreeSplitter.find_subtrees(
               "./test/fixtures/tree_split_exports_multiple_matches.erl",
               @default_paths,
               TestMatcher.all()
             )
  end

  test "keeps only the necessary exports (individual lines)" do
    assert {
             :ok,
             [
               %{
                 ast: [
                   {:attribute, 0, :file, {'./test/fixtures/tree_split_exports.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports},
                   {:attribute, 0, :export, [foo: 1]},
                   {:function, 0, :foo, 1,
                    [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :baz}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo, 1}
               },
               %{
                 ast: [
                   {:attribute, 0, :file, {'./test/fixtures/tree_split_exports.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports},
                   {:attribute, 0, :export, [foo: 0]},
                   {:function, 0, :foo, 0, [{:clause, 0, [], [], [{:atom, 0, :bar}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo, 0}
               }
             ]
           } =
             TreeSplitter.find_subtrees(
               "./test/fixtures/tree_split_exports.erl",
               @default_paths,
               TestMatcher.match("foo")
             )
  end

  test "keeps only the necessary exports (multiline)" do
    assert {
             :ok,
             [
               %{
                 ast: [
                   {:attribute, 0, :file,
                    {'./test/fixtures/tree_split_exports_multiline.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports},
                   {:attribute, 0, :export, [foo: 1]},
                   {:function, 0, :foo, 1,
                    [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :baz}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo, 1}
               },
               %{
                 ast: [
                   {:attribute, 0, :file,
                    {'./test/fixtures/tree_split_exports_multiline.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports},
                   {:attribute, 0, :export, [foo: 0]},
                   {:function, 0, :foo, 0, [{:clause, 0, [], [], [{:atom, 0, :bar}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo, 0}
               }
             ]
           } =
             TreeSplitter.find_subtrees(
               "./test/fixtures/tree_split_exports_multiline.erl",
               @default_paths,
               TestMatcher.match("foo")
             )
  end

  test "finds many matches starting with the search string" do
    assert {
             :ok,
             [
               %{
                 ast: [
                   {:attribute, 0, :file,
                    {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                   {:attribute, 0, :export, [foo_3_test: 1]},
                   {:function, 0, :foo_3_test, 1,
                    [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :baz}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo_3_test, 1}
               },
               %{
                 ast: [
                   {:attribute, 0, :file,
                    {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                   {:attribute, 0, :export, [foo_2_test: 1]},
                   {:function, 0, :foo_2_test, 1,
                    [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :baz}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo_2_test, 1}
               },
               %{
                 ast: [
                   {:attribute, 0, :file,
                    {'./test/fixtures/tree_split_exports_multiple_matches.erl', 1}},
                   {:attribute, 0, :module, :tree_split_exports_multiple_matches},
                   {:attribute, 0, :export, [foo_1_test: 1]},
                   {:function, 0, :foo_1_test, 1,
                    [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :bar}]}]},
                   {:eof, 0}
                 ],
                 name: {:foo_1_test, 1}
               }
             ]
           } =
             TreeSplitter.find_subtrees(
               "./test/fixtures/tree_split_exports_multiple_matches.erl",
               @default_paths,
               TestMatcher.match("foo")
             )
  end
end
