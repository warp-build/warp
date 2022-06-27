defmodule ZapTest do
  use ExUnit.Case
  doctest Zap

  test "greets the world" do
    assert Zap.hello() == :world
  end
end
