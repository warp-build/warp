defmodule Analyzer.GetAst do
  require Logger

  def get_ast(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    cond do
      Path.extname(req.file) in [".erl", ".hrl"] -> do_get_erl_ast(req)
    end
  end

  defp do_get_erl_ast(req) do
    include_paths =
      Analyzer.ErlangHacks.IncludePaths.from_file_and_deps(req.file, req.dependencies)
      # NOTE(@ostera): this is needed because the tree we will generate after
      # the `:erl_analyzer.subtree` call will contain include paths that use these prefixes
      # and preserves the kind of string they are. So since we are in Elixir, it will all be binaries.
      #
      #  HOWEVER, `:erl_prettypr.format` doesn't like include forms with binaries like this:
      #     {:attribute, 1, :file, {"app/include/header.hrl", 1}}
      |> Enum.map(fn path -> :binary.bin_to_list(path) end)

    symbol = build_symbol(req.symbol.sym)

    {:ok, result} =
      :erl_tree_splitter.split(%{
        file: req.file,
        include_paths: include_paths,
        symbol: symbol
      })

    IO.inspect(result)

    response = handle_result(req, result)

    Build.Warp.Codedb.GetAstResponse.new(response: response)
    |> IO.inspect()
  end

  @doc """
  Put together a Symbol that can be used to split the tree by the `:erl_tree_splitter` module.

  """
  defp build_symbol({:all, true}), do: :all

  defp build_symbol({:named, sym}) do
    [f, a] =
      case sym |> String.split("/") do
        [sym] -> [sym, "0"]
        fa -> fa
      end

    {:named, {String.to_atom(f), String.to_integer(a)}}
  end

  defp handle_result(req, {:missing_includes, deps}) do
    deps =
      deps
      |> Enum.uniq()
      |> Enum.map(fn dep ->
        req = {:file, Build.Warp.FileRequirement.new(path: dep)}
        Build.Warp.Requirement.new(requirement: req)
      end)

    resp =
      Build.Warp.Codedb.GetAstMissingDepsResponse.new(
        file: req.file,
        symbol: req.symbol,
        dependencies: deps
      )

    {:missing_deps, resp}
  end

  defp handle_result(req, {:completed, ast}) do
    source =
      case req.symbol.sym do
        {:all, true} ->
          File.read!(req.file)

        {:named, _} ->
          ast
          |> :erl_syntax.form_list()
          |> :erl_prettypr.format(encoding: :utf8)
          |> :binary.list_to_bin()
      end

    {:ok,
     Build.Warp.Codedb.GetAstSuccessResponse.new(
       status: :STATUS_OK,
       file: req.file,
       symbol: Build.Warp.Symbol.new(sym: req.symbol.sym),
       ast: Kernel.inspect(ast),
       source: source
     )}
  end
end
