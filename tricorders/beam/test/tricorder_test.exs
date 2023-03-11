defmodule TricorderTest do
  use ExUnit.Case
  doctest Tricorder

  test "greets the world" do
    assert Tricorder.hello() == :world
  end
end
