defmodule Tricorder.Application do
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    {port, _} =
      Application.get_env(:tricorder, :grpc_port)
      |> Integer.parse()

    children = [
      {GRPC.Server.Supervisor, {Tricorder.Grpc.Endpoint, port}}
    ]

    opts = [strategy: :one_for_one, name: Tricorder.Supervisor]

    Supervisor.start_link(children, opts)
  end
end
