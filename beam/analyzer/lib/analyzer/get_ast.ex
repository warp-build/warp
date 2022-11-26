defmodule Analyzer.GetAst do
  require Logger

  def get_ast(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    cond do
      Path.extname(req.file) in [".erl", ".hrl"] -> do_get_erl_ast(req)
    end
  end

  defp do_get_erl_ast(req) do
    file = req.file
    {:ok, %{^file => result}} = :erl_analyzer.analyze([file], _ModMap = %{}, _IncludePaths = [])

    symbol =
      case req.symbol.sym do
        {:all, true} ->
          :all

        {:named, sym} ->
          [f, a] =
            case sym |> String.split("/") do
              [sym] -> [sym, "0"]
              fa -> fa
            end

          {:named, {String.to_atom(f), String.to_integer(a)}}
      end

    {:ok, ast} = :erl_analyzer.subtree(result, symbol)

    source =
      ast
      |> :erl_syntax.form_list()
      |> :erl_prettypr.format(encoding: :utf8)
      |> :binary.list_to_bin()

    Build.Warp.Codedb.GetAstResponse.new(
      status: :STATUS_OK,
      file: file,
      symbol: Build.Warp.Symbol.new(sym: req.symbol.sym),
      ast: Kernel.inspect(ast),
      source: source
    )
  end
end
