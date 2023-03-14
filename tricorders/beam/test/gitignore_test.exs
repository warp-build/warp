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
             "test/fixtures/rebar.lock",
             "test/fixtures/mix.lock"
           ] = Gitignore.find(gitignore, "test/fixtures", fn x -> {:keep, x} end)
  end
end
