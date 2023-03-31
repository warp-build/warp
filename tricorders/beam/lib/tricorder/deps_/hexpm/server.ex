defmodule Tricorder.Deps.Hexpm.Server do
  require Logger

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :none, name: __MODULE__)
  end

  ## Callbacks

  @impl true
  def init(_) do
    config =
      :hex_core.default_config()
      |> Map.merge(%{
        http_user_agent_fragment: "(tools.warp.build/hexpm/resolver)",
        http_adapter: {Tricorder.Deps.Hexpm.Resolver, %{}}
      })

    {:ok, %{config: config}}
  end

  @impl true
  def handle_call({:get_package, name}, _from, state) do
    pkg_name = Atom.to_string(name)
    {:ok, {_, _, resp}} = :hex_api_package.get(state.config, pkg_name)
    {:reply, {:ok, resp}, state}
  end

  @impl true
  def handle_cast(_msg, state), do: {:noreply, state}
end
