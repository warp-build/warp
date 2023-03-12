defmodule Tricorder.Application do
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    children = [
      {GRPC.Server.Supervisor, {Tricorder.Grpc.Endpoint, 21000}}
    ]

    opts = [strategy: :one_for_one, name: Tricorder.Supervisor]

    Supervisor.start_link(children, opts)
  end
end

