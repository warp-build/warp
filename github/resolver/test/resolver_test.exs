defmodule ResolverTest do
  use ExUnit.Case
  doctest Resolver

  test "greets the world" do
    assert Resolver.hello() == :world
  end
end
