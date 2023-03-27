defmodule Tricorder.Analysis.Erlang do
  alias __MODULE__
  alias Tricorder.Signatures

  require Erlang.Cerl

  def analyze(file, test_matcher, %{include_paths: include_paths, code_paths: code_paths}) do
    with {:ok, ast} <- Erlang.Ast.parse(file, include_paths),
         {:ok, result} <- analyze_source(file, test_matcher, include_paths, code_paths, ast) do
      {:ok, result}
    else
      diag = {:missing_dependencies, _} -> {:ok, diag}
    end
  end

  def analyze_source(file, test_matcher, include_paths, code_paths, ast) do
    analysis = Erlang.Ast.scan(ast)

    case Path.extname(file) do
      ".hrl" ->
        analyze_hrl(ast, file, analysis)

      ".erl" ->
        analyze_erl(file, test_matcher, include_paths, code_paths, ast, analysis)
    end
  end

  def analyze_hrl(ast, file, analysis) do
    signatures = [Signatures.erlang_header_library(file, analysis.includes)]
    {:ok, {:completed, signatures}}
  end

  def analyze_erl(file, test_matcher, include_paths, code_paths, ast, src_analysis) do
    with {:ok, _, _} <- Erlang.Cerl.compile(file, include_paths, code_paths) do
      modules =
        (src_analysis.remote_functions ++
           src_analysis.remote_types ++
           src_analysis.imported_mods ++ src_analysis.parse_transforms)
        |> Enum.uniq()

      includes = (src_analysis.includes ++ src_analysis.missing_includes) |> Enum.uniq()

      signatures =
        [Signatures.erlang_library(file, modules, includes)] ++
          Erlang.CommonTest.suites(file, test_matcher, modules, includes, src_analysis)

      {:ok, {:completed, signatures}}
    end
  end
end
