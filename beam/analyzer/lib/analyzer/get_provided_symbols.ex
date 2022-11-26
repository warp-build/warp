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
    {:ok, %{^file => result}} = :erl_analyzer.analyze([file], _ModMap = %{}, _IncludePaths = [])

    requirement =
      case Path.extname(req.file) do
        ".erl" ->
          {:ok, mod_name} = :erl_stdlib.file_to_module(req.file)
          mod_name = mod_name |> Atom.to_string()
          {:symbol, Build.Warp.SymbolRequirement.new(raw: mod_name, kind: "module")}

        ".hrl" ->
          {:file, Build.Warp.FileRequirement.new(path: req.file)}
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
      [Build.Warp.Requirement.new(requirement: requirement)] ++
        exported_fns ++
        exported_types

    Build.Warp.Codedb.GetProvidedSymbolsResponse.new(
      skipped: false,
      file: req.file,
      provides: provides
    )
  end
end
