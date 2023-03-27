defmodule GitignoreTest do
  use ExUnit.Case
  doctest Gitignore

  test "matches common patterns" do
    gitignore = Gitignore.read("./test/fixtures/.gitignore")
    assert Gitignore.should_ignore?(gitignore, "test/file_test.ex")
  end

  test "finds files respecting gitignore dirs" do
    gitignore = Gitignore.read("./test/fixtures/.gitignore")

    assert [
             "test/fixtures/.gitignore",
             "test/fixtures/ct_suite_with_groups_SUITE.erl",
             "test/fixtures/ct_suite_without_groups_SUITE.erl",
             "test/fixtures/direct_deps.erl",
             "test/fixtures/direct_type_deps.erl",
             "test/fixtures/found_header.hrl",
             "test/fixtures/found_includes.erl",
             "test/fixtures/imported_deps.erl",
             "test/fixtures/missing_includes.erl",
             "test/fixtures/missing_parse_transforms.erl",
             "test/fixtures/mix.lock",
             "test/fixtures/prop_verl.erl",
             "test/fixtures/proper.hrl",
             "test/fixtures/proper_common.hrl",
             "test/fixtures/proper_internal.hrl",
             "test/fixtures/proper_transformer.beam",
             "test/fixtures/proper_transformer.erl",
             "test/fixtures/proper_unused_imports_remover.beam",
             "test/fixtures/proper_unused_imports_remover.erl",
             "test/fixtures/rebar.config",
             "test/fixtures/rebar.lock"
           ] =
             Gitignore.find(gitignore, "test/fixtures", fn x -> {:keep, x} end)
             |> Enum.sort()
  end
end
