defmodule LibraryTest do
  use ExUnit.Case
  doctest Library

  test "greets the world" do
    assert Library.hello() == :world
  end
end
