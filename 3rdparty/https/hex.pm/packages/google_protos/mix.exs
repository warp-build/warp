defmodule GoogleProtos.MixProject do
  use Mix.Project

  def project do
    [
      app: :google_protos,
      name: "Google Protos",
      version: "0.3.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Protos by Google",
      package: package()
    ]
  end

  def application do
    []
  end

  defp deps do
    [
      {:protobuf, "~> 0.10"},
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  defp package do
    [
      maintainers: ["Tony Han", "Andrea Leopardi"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/elixir-protobuf/google-protos"},
      files: ~w(mix.exs README.md lib LICENSE)
    ]
  end
end
