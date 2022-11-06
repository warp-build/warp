defmodule Analyzer.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: Analyzer.Worker.start_link(arg)
      # {Analyzer.Worker, arg}
      {GRPC.Server.Supervisor, {Analyzer.Endpoint, 21000, start_server: true}}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Analyzer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
