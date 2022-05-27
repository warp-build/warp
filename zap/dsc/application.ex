defmodule Zap.Dsc.Application do
  @moduledoc false

  require Logger
  use Application

  def start(_type, _args) do
    Logger.info("Starting distributed shared cache...")

    children = [
      # {Zap.Dsc.SharedCache, []}
    ]

    opts = [strategy: :one_for_one, name: Zap.Dsc.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
