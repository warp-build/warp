defmodule Tricorder.Application do
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    :code.delete(Tricorder.Project)
    :code.purge(Tricorder.Project)

    children =
      if Application.get_env(:tricorder, :start_server, false) do
        port = Application.get_env(:tricorder, :grpc_port) |> String.to_integer()
		workspace_root = Application.get_env(:tricorder, :workspace_root)

        [
          {GRPC.Server.Supervisor, {Tricorder.Grpc.Endpoint, port}},
          %{
            id: Tricorder.Deps.Server,
            start: {Tricorder.Deps.Server, :start_link, [workspace_root]}
          }
        ]
      else
        []
      end

    opts = [strategy: :one_for_one, name: Tricorder.Supervisor]

    Supervisor.start_link(children, opts)
  end
end
