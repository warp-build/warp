defmodule Tricorder.Grpc.Ops.GetAst do
  require Logger

  alias Tricorder.Analysis.Erlang
  alias Tricorder.Analysis
  alias Tricorder.Deps

  def get_ast(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    matcher = Analysis.TestMatcher.from_parts(req.test_matcher)
    Logger.info("Using test_matcher: #{inspect(matcher)}")

    paths =
      Analysis.Paths.build_paths(
        req.file,
        req.dependencies
      )

    Logger.info("Splitting tree with paths: #{inspect(paths)}")

    {:ok, results} =
      Erlang.TreeSplitter.find_subtrees(
        req.file,
        %{
          include_paths: paths.include_paths
        },
        matcher
      )

    subtrees = handle_result(req, results)

    Logger.debug("Generated #{Enum.count(subtrees)} ASTs response")

    response = {:ok, Build.Warp.Tricorder.GetAstSuccessResponse.new(subtrees: subtrees)}
    Build.Warp.Tricorder.GetAstResponse.new(response: response)
  end

  defp handle_result(req, subtrees) do
    subtrees
    |> Enum.map(fn %{name: {name, _arity}, ast: ast} ->
      source =
        ast
        |> :erl_syntax.form_list()
        |> :erl_prettypr.format(encoding: :utf8)
        |> :binary.list_to_bin()

      Build.Warp.Tricorder.AstSubtree.new(
        file: req.file,
        ast: Kernel.inspect(ast, limit: 100_000_000),
        source_chunk: source,
        signature_name: Atom.to_string(name)
      )
    end)
  end
end
