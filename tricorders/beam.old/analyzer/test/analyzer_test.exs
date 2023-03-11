defmodule AnalyzerTest do
  use ExUnit.Case
  doctest Analyzer

  test "greets the world" do
    assert Analyzer.hello() == :world
  end
end
