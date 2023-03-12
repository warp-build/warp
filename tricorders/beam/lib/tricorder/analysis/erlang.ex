defmodule Tricorder.Analysis.Erlang do
  alias Tricorder.Analysis.Erlang
  alias Tricorder.Signatures

  def analyze(file, %{include_paths: include_paths, code_paths: code_paths}) do
    with {:ok, ast} <- Erlang.Ast.parse(file, include_paths),
         {:ok, src} <- analyze_source(file, include_paths, code_paths, ast) do
    end
  end

  def analyze_source(file, include_paths, code_paths, ast) do
    analysis = Erlang.Ast.scan(ast)

    case Path.extname(file) do
      "hrl" ->
        analyze_hrl(ast, file, analysis)

      "erl" ->
        analyze_erl(file, include_paths, code_paths, ast, analysis)
    end
  end

  def analyze_hrl(ast, file, analysis) do
    signatures = [Signatures.erlang_header_library(file, analysis.includes)]
    {:ok, signatures}
  end

  def analyze_erl(file, include_paths, code_paths, ast, src_analysis) do
    with {:ok, _, _} <- Erlang.Cerl.compile(file, include_paths, code_paths) do
      modules =
        for {mod, _, _} <-
              src_analysis.remote_functions ++
                src_analysis.remote_types ++
                src_analysis.imported_mods ++ src_analysis.parse_transforms do
          mod
        end
        |> Enum.uniq()

      includes = (src_analysis.includes ++ src_analysis.missing_includes) |> Enum.uniq()

      [Signatures.erlang_library(file, modules, includes)] ++
        ct_suites(file, modules, includes, src_analysis)
    end
  end

  def ct_suites(file, modules, includes, src_analysis) do
    if String.ends_with?(file, "_SUITE.erl") do
      mod_name = Path.basename(file, ".erl") |> String.to_atom()
      cases = apply(mod_name, :all, [])

      for case_name <- cases do
        Siganture.erlang_test(%{
          name: "#{file}:#{case_name}",
          test: file,
          modules: modules,
          includes: includes,
          cases: [case_name]
        })
      end
    else
      []
    end
  end
end
