defmodule Analyzer.MixProject do
  use Mix.Project

  def project do
    [
      app: :analyzer_service,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :grpc, :protobuf],
      mod: {Analyzer.Application, []}
    ]
  end

  defp deps do
    [
      {:castore, "0.1.18"},
      {:google_protos, "~> 0.3"},
      {:grpc, "~> 0.5"},
      {:hex_core, "~0.9.0"},
      {:mint, "1.4.2"},
      {:protobuf, "~> 0.10"}
    ]
  end
end
