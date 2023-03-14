defmodule GitignoreTest do
  use ExUnit.Case
  doctest Gitignore

  test "matches common patterns" do
    gitignore = Gitignore.read("./test/fixtures/.gitignore")
    assert Gitignore.should_ignore?(gitignore, "/test/")
    assert Gitignore.should_ignore?(gitignore, "test/file_test.ex")
  end

  test "finds files respecting gitignore dirs" do
    gitignore = Gitignore.read("./test/fixtures/.gitignore")

    assert ["./test/fixtures/mix.lock", "./mix.lock"] =
             Gitignore.find(gitignore, ".", "mix.lock", fn x -> x end)
  end
end
