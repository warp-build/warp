defmodule Tricorder.Analysis.Erlang.TreeSplitter do
  require Logger
  alias Tricorder.Analysis.Erlang
  alias Tricorder.Analysis.TestMatcher

  def find_subtrees(file, %{include_paths: include_paths}, matcher) do
    with {:ok, ast} <- Erlang.Ast.parse(file, include_paths),
         subtrees <- subtree(ast, matcher) |> clean_subtrees do
      Logger.info("Final tree: #{inspect(subtrees, pretty: true, limit: 1_000_000)}")
      {:ok, subtrees}
    else
      diag = {:missing_dependencies, _} -> {:ok, diag}
    end
  end

  def clean_subtrees(subtrees) do
    subtrees
    |> Enum.map(fn subtree ->
      %{subtree | ast: Erlang.Ast.CleanLoc.clean(subtree.ast)}
    end)
  end

  def subtree(ast, matcher) do
    find_symbols(ast, matcher)
    |> Enum.map(fn {name, symbol} ->
      %{
        name: name,
        ast: slice_tree(ast, symbol, name, matcher)
      }
    end)
  end

  def find_symbols(ast, matcher) do
    Logger.debug("Searching for symbol: #{inspect(matcher)}")

    :erl_visitor.walk(
      ast,
      _acc = [],
      fn
        ast = {:function, _loc, name, arity, _body}, acc ->
          if TestMatcher.matches?(matcher, name) do
            Logger.debug("Found symbol: #{name}/#{arity}")
            [{{name, arity}, ast} | acc]
          else
            if {name, arity} == matcher do
              Logger.debug("Found symbol: #{name}/#{arity}")
              [{{name, arity}, ast} | acc]
            else
              Logger.debug("Skipped symbol: #{name}/#{arity}")
              acc
            end
          end

        _ast, acc ->
          acc
      end
    )
  end

  def slice_tree(ast, symbol, name, matcher) do
    Logger.debug("Slicing tree for #{inspect(name)} with symbol: #{inspect(symbol)}")
    deps = symbol_deps(ast, symbol)
    Logger.debug("Stripping tree...")
    strip_tree(ast, [name | deps], [])
  end

  def strip_tree([], _symbols, acc), do: acc |> Enum.reverse()

  def strip_tree([{:attribute, loc, :export, exports} | ast], symbols, acc) do
    Logger.debug("Found exports: #{inspect(exports, pretty: true)}")
    exports = exports |> Enum.filter(fn export -> export in symbols end)

    if Enum.empty?(exports) do
      strip_tree(ast, symbols, acc)
    else
      strip_tree(ast, symbols, [{:attribute, 0, :export, exports} | acc])
    end
  end

  def strip_tree([node = {:attribute, _, _, _} | ast], symbols, acc) do
    strip_tree(ast, symbols, [node | acc])
  end

  def strip_tree([node = {:function, _loc, name, arity, _body} | ast], symbols, acc) do
    if {name, arity} in symbols do
      # io:format("found function: ~p/~p\n", [name, arity])
      strip_tree(ast, symbols, [node | acc])
    else
      # io:format("skipped function: ~p/~p\n", [name, arity])
      strip_tree(ast, symbols, acc)
    end
  end

  def strip_tree([node | ast], symbols, acc) do
    strip_tree(ast, symbols, [node | acc])
  end

  def symbol_deps(ast, symbol) do
    deps = extract_symbol_deps(symbol)

    rec_deps =
      for dep <- deps do
        Logger.debug("Subdeps for #{inspect(dep)}")

        find_symbols(ast, dep)
        |> Enum.map(fn dep_ast -> symbol_deps(ast, dep_ast) end)
      end

    deps = List.flatten([deps | rec_deps])
    Logger.debug("Symbol dependencies: #{inspect(deps)}")
    deps
  end

  def extract_symbol_deps(ast) do
    :erl_visitor.walk(
      ast,
      _acc = [],
      fn
        _ast = {:call, _Loc, {:atom, _Loc2, name}, args}, acc ->
          [{name, length(args)} | acc]

        _ast, acc ->
          acc
      end
    )
  end
end
