defmodule Tricorder.CLI do
  require Logger
  def main(["start", port]) do
    Logger.info("Starting Tricorder application on port #{port}")

    Application.put_env(:tricorder, :grpc_port, port)
    :ok = Application.stop(:tricorder)
    {:ok, _} = Application.ensure_all_started(:tricorder)

    loop()
  end

  defp loop(), do: loop()
end
