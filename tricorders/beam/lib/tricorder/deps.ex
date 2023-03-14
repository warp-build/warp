defmodule Tricorder.Deps do
  require Logger

  def scan() do
    GenServer.cast(Tricorder.Deps.Server, :scan)
  end

  def get_all() do
    GenServer.call(Tricorder.Deps.Server, :get_all)
  end

  def loaded?() do
    GenServer.call(Tricorder.Deps.Server, :loaded?, :infinity)
  end

  def find_header(hrl) do
    Logger.debug("find_header: #{hrl}")

    case Path.split(hrl) do
      [lib_name, "include", _] ->
        lib_name = lib_name |> String.to_atom()
        GenServer.call(Tricorder.Deps.Server, {:get, lib_name})

      _ ->
        {:error, :not_found}
    end
  end
end
