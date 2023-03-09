defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.5.0"

  def project do
    [
      app: :grpc,
      version: @version,
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: "The Elixir implementation of gRPC",
      docs: [
        extras: ["README.md"],
        main: "readme",
        source_ref: "v#{@version}",
        source_url: "https://github.com/elixir-grpc/grpc"
      ],
      dialyzer: [
        plt_add_deps: :apps_tree,
        plt_add_apps: [:iex, :mix, :ex_unit],
        list_unused_filters: true,
        plt_file: {:no_warn, "_build/#{Mix.env()}/plts/dialyzer.plt"}
      ],
      xref: [exclude: [IEx]]
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:cowboy, "~> 2.9"},
      # This is the same as :gun 2.0.0-rc.2,
      # but we can't depend on an RC for releases
      {:gun, "~> 2.0.1", hex: :grpc_gun},
      {:cowlib, "~> 2.11"},
      {:protobuf, "~> 0.10", only: [:dev, :test]},
      {:ex_doc, "~> 0.28", only: :dev},
      {:inch_ex, "~> 2.0", only: [:dev, :test]},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false}
    ]
  end

  defp package do
    %{
      maintainers: ["Bing Han"],
      licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/elixir-grpc/grpc"},
      files: ~w(mix.exs README.md lib src config LICENSE .formatter.exs)
    }
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end