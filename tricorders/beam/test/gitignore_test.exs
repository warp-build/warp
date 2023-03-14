defmodule GitignoreTest do
  use ExUnit.Case
  doctest Gitignore

  test "matches common patterns" do
    gitignore = Gitignore.read("./test/fixtures/.gitignore")
    assert Gitignore.should_ignore?(gitignore, "/test/")
    assert Gitignore.should_ignore?(gitignore, "test/file_test.ex")
  end
end
