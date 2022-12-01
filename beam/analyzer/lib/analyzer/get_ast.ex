defmodule Analyzer.GetAst do
  require Logger

  def get_ast(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    cond do
      Path.extname(req.file) in [".erl", ".hrl"] -> do_get_erl_ast(req)
    end
  end

  defp do_get_erl_ast(req) do
    # FIXME(@ostera): this is a hack to make the source analyzer find headers
    #
    # NOTE(@ostera): we don't want to discover things on the FS root
    parts = case req.file |> Path.dirname() |> Path.split() do
      ["/" | parts] -> parts
      parts -> parts
    end

    include_paths =
      [
        "include",
        Enum.map(req.dependencies, fn dep ->
          Path.dirname(Path.dirname(dep.store_path))
        end),
        for i <- 0..(Enum.count(parts) - 1) do
          path =
            case Enum.take(parts, i) do
              [] -> "."
              parts -> parts |> Path.join()
            end

          [path, Path.join(path, "include")]
        end
      ]
      |> List.flatten()
      |> Enum.sort()
      |> Enum.uniq()

      # NOTE(@ostera): this is needed because the tree we will generate after
      # the `:erl_analyzer.subtree` call will contain include paths that use these prefixes
      # and preserves the kind of string they are. So since we are in Elixir, it will all be binaries.
      #
      #  HOWEVER, `:erl_prettypr.format` doesn't like include forms with binaries like this:
      #     {:attribute, 1, :file, {"app/include/header.hrl", 1}}
      |> Enum.map(fn path -> :binary.bin_to_list(path) end)

    Logger.info("splitting ast using include paths:")

    for p <- include_paths do
      IO.inspect(p)
    end

    file = req.file
    {:ok, %{^file => result}} = :erl_analyzer.analyze([file], _ModMap = %{}, include_paths)

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
