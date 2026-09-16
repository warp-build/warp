defmodule Tricorder.CLI do
  require Logger

  def main(["start", port, workspace_root | _]) do
    Logger.info("Starting Tricorder application on port #{port}")

    Application.put_env(:tricorder, :start_server, true)
    Application.put_env(:tricorder, :grpc_port, port)
    Application.put_env(:tricorder, :workspace_root, workspace_root)
    :ok = Application.stop(:tricorder)
    {:ok, _} = Application.ensure_all_started(:tricorder)

    Tricorder.Deps.scan()

    loop()
  end

  defp loop(), do: loop()
end
