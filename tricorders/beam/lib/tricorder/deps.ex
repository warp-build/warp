defmodule Tricorder.Deps do
  def scan() do
    GenServer.cast(Tricorder.Deps.Server, :scan)
  end

  def loaded?() do
    GenServer.call(Tricorder.Deps.Server, :loaded?, :infinity)
  end
end
