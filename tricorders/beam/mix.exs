defmodule Tricorder.MixProject do
  use Mix.Project

  def project do
    [
      app: :tricorder,
      version: "0.0.0",
      elixir: "~> 1.13",
      start_permanent: false,
      deps: deps(),
      escript: escript()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :grpc, :protobuf],
      mod: {Tricorder.Application, []},
    ]
  end

  defp escript do
    [
      main_module: Tricorder.CLI
    ]
  end

  defp deps do
    [
      {:castore, "~> 0.1"},
      {:google_protos, "~> 0.3"},
      {:grpc, "~> 0.5"},
      {:hex_core, "~> 0.9"},
      {:jason, "~> 1.4"},
      {:mint, "~> 1.4"},
      {:protobuf, "~> 0.10"}
    ]
  end
end
