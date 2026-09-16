defmodule Tricorder.Analysis.Elixir do
  alias Tricorder.Signatures

  def analyze_script(file, %{
        include_paths: include_paths,
        code_paths: code_paths
      }) do
    signatures = [
      Signatures.elixir_script(file)
    ]

    {:ok, {:completed, signatures}}
  end

  def analyze_lib(file, %{
        include_paths: include_paths,
        code_paths: code_paths
      }) do
    signatures = [
      Signatures.elixir_library(file)
    ]

    {:ok, {:completed, signatures}}
  end
end
