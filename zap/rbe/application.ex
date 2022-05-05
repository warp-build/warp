defmodule Zap.Rbe.Application do
  @moduledoc false

  require Logger
  use Application

  def start(_type, _args) do
    Logger.info("Starting remote build engine...")

    # List all child processes to be supervised
    children = [
      # {Zap.Rbe.BuildQueue, []}
    ]

    opts = [strategy: :one_for_one, name: Zap.Rbe.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
