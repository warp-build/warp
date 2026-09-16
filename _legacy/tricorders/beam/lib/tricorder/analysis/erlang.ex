defmodule Tricorder.Analysis.Erlang do
  alias __MODULE__
  alias Tricorder.Signatures

  require Logger
  require Erlang.Cerl

  def analyze(file, test_matcher, paths = %{include_paths: _, code_paths: _}) do
    Logger.info("Analyzing Erlang file: #{file}")

    with {:ok, ast} <- Erlang.Ast.parse(file, paths.include_paths),
         {:ok, result} <- analyze_source(file, test_matcher, paths, ast) do
      {:ok, result}
    else
      diag = {:missing_dependencies, _} -> {:ok, diag}
    end
  end

  def analyze_source(file, test_matcher, paths, ast) do
    Logger.info("Scanning Erlang AST: #{file}")
    analysis = Erlang.Ast.scan(file, ast)

    Logger.debug("Scan Result: #{inspect(analysis, pretty: true, limit: :infinity)}")

    case Path.extname(file) do
      ".hrl" ->
        analyze_hrl(ast, file, analysis)

      ".erl" ->
        analyze_erl(file, test_matcher, paths, ast, analysis)
    end
  end

  def analyze_hrl(ast, file, analysis) do
    Logger.info("Analyzing Erlang Header AST: #{file}")
    signatures = [Signatures.erlang_header_library(file, analysis.includes)]
    {:ok, {:completed, signatures}}
  end

  def analyze_erl(file, test_matcher, paths, ast, src_analysis) do
    Logger.info("Analyzing Erlang Module AST: #{file}")

    with {:ok, _mod, _bytecode, _core} <- Erlang.Cerl.compile(file, paths) do
      modules =
        (src_analysis.remote_functions ++
           src_analysis.remote_types ++
           src_analysis.imported_mods ++ src_analysis.parse_transforms)
        |> Enum.uniq()

      includes = (src_analysis.includes ++ src_analysis.missing_includes) |> Enum.uniq()

      if Erlang.CommonTest.is_test_suite?(file) do
        with signatures when is_list(signatures) <-
               Erlang.CommonTest.suites(file, test_matcher, modules, includes, paths) do
          {:ok, {:completed, signatures}}
        end
      else
        signatures = [Signatures.erlang_library(file, modules, includes)]
        {:ok, {:completed, signatures}}
      end
    end
  end

  def module_name_for_file(file), do: Path.basename(file, ".erl") |> String.to_atom()
end
