defmodule Tricorder.Analysis.Erlang do
  alias __MODULE__
  alias Tricorder.Signatures

  require Erlang.Cerl

  def analyze(file, %{include_paths: include_paths, code_paths: code_paths}) do
    with {:ok, ast} <- Erlang.Ast.parse(file, include_paths) do
      analyze_source(file, include_paths, code_paths, ast)
    else
      diag = {:missing_dependencies, _} -> {:ok, diag}
    end
  end

  def analyze_source(file, include_paths, code_paths, ast) do
    analysis = Erlang.Ast.scan(ast)

    case Path.extname(file) do
      ".hrl" ->
        analyze_hrl(ast, file, analysis)

      ".erl" ->
        analyze_erl(file, include_paths, code_paths, ast, analysis)
    end
  end

  def analyze_hrl(ast, file, analysis) do
    signatures = [Signatures.erlang_header_library(file, analysis.includes)]
    {:ok, {:completed, signatures}}
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

      signatures =
        [Signatures.erlang_library(file, modules, includes)] ++
          ct_suites(file, modules, includes, src_analysis)

      {:ok, {:completed, signatures}}
    end
  end

  def ct_suites(file, modules, includes, src_analysis) do
    if String.ends_with?(file, "_SUITE.erl") do
      {:ok, mod} = :compile.noenv_file(:binary.bin_to_list(file), @default_compile_opts)
      cases = mod.all()
      :code.delete(mod)
      :code.purge(mod)
      false = :code.is_loaded(mod)

      for case_name <- cases do
        Signatures.erlang_test(file, case_name, modules, includes)
      end
    else
      []
    end
  end
end
