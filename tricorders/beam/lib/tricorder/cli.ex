defmodule Tricorder.CLI do
  def main(["start", port]) do
    Application.put_env(:tricorder, :grpc_port, port)
    {:ok, pid} = Tricorder.Application.start(:normal, [])
    loop()
  end

  defp loop(), do: loop()
end
