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
      {:google_protos, "~> 0.3"},
      {:grpc, "~> 0.5"},
      {:protobuf, "~> 0.10"},
    ]
  end
end
