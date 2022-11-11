defmodule Resolver.MixProject do
  use Mix.Project

  def project do
    [
      app: :resolver_service,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:google_protos, "~> 0.3"},
      {:grpc, "~> 0.5"},
      {:protobuf, "~> 0.10"},
    ]
  end
end
