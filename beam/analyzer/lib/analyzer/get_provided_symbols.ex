defmodule Analyzer.GetProvidedSymbols do
  require Logger

  def get_provided_symbols(req, _stream) do
    cond do
      Path.extname(req.file) in [".erl", ".hrl"] ->
        do_get_provided_symbols(req)

      true ->
        Logger.info("Skipped #{req.file}")
        Build.Warp.Codedb.GetProvidedSymbolsResponse.new(skipped: true, file: req.file)
    end
  end

  defp do_get_provided_symbols(req) do
    Logger.info("Analyzing: #{req.file}")

    file = req.file

    {:ok, ast} = :erl_ast.parse_file(file, _include_paths = [])
    {:ok, result} = :erl_analyzer.analyze(%{ast: ast, file: file, include_paths: []})

    requirements =
      case Path.extname(req.file) do
        ".erl" ->
          {:ok, mod_name} = :erl_stdlib.file_to_module(req.file)
          mod_name = mod_name |> Atom.to_string()
          req = {:symbol, Build.Warp.SymbolRequirement.new(raw: mod_name, kind: "module")}
          [Build.Warp.Requirement.new(requirement: req)]

        ".hrl" ->
          parts = req.file |> Path.split()

          for i <- 0..(Enum.count(parts) - 1) do
            path = Enum.drop(parts, i) |> Path.join()
            req = {:file, Build.Warp.FileRequirement.new(path: path)}
            Build.Warp.Requirement.new(requirement: req)
          end
      end

    exported_fns =
      :erl_analyzer.exported_functions(result)
      |> Enum.map(fn {m, f, a} ->
        symbol =
          Build.Warp.SymbolRequirement.new(
            raw: "#{Atom.to_string(m)}:#{Atom.to_string(f)}/#{a}",
            kind: "function"
          )

        Build.Warp.Requirement.new(requirement: {:symbol, symbol})
      end)

    exported_types =
      :erl_analyzer.exported_types(result)
      |> Enum.map(fn {m, f, a} ->
        symbol =
          Build.Warp.SymbolRequirement.new(
            raw: "#{Atom.to_string(m)}:#{Atom.to_string(f)}/#{a}",
            kind: "type"
          )

        Build.Warp.Requirement.new(requirement: {:symbol, symbol})
      end)

    provides =
      requirements ++
        exported_fns ++
        exported_types

    Build.Warp.Codedb.GetProvidedSymbolsResponse.new(
      skipped: false,
      file: req.file,
      provides: provides
    )
  end
end
