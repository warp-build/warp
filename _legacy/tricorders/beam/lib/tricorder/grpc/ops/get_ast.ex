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

    response = handle_result(req, results)

    Build.Warp.Tricorder.GetAstResponse.new(response: response)
  end

  defp handle_result(req, {:missing_dependencies, deps}) do
    includes = Map.get(deps, :includes, []) |> handle_includes

    modules = Map.get(deps, :modules, []) |> handle_modules

    resp =
      Build.Warp.Tricorder.GetAstMissingDepsResponse.new(
        file: req.file,
        requirements: includes ++ modules
      )

    {:missing_deps, resp}
  end

  defp handle_result(req, subtrees) do
    subtrees =
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

    Logger.debug("Generated #{Enum.count(subtrees)} ASTs response")

    {:ok, Build.Warp.Tricorder.GetAstSuccessResponse.new(subtrees: subtrees)}
  end

  defp handle_includes(includes) do
    includes
    |> Enum.uniq()
    |> Enum.reject(fn hrl -> Deps.is_standard_header?(hrl) end)
    |> Enum.map(fn hrl ->
      req =
        case Deps.find_header(hrl) do
          {:ok, dep} ->
            {:url,
             Build.Warp.UrlRequirement.new(
               url: dep.url,
               subpath: dep.name
             )}

          _ ->
            hrl = clean_warp_store_path(hrl)

            glob = Path.wildcard("./**/#{hrl}")
            Logger.info("Found module #{hrl} in #{glob}")

            req =
              case glob do
                [file | _] -> {:file, Build.Warp.FileRequirement.new(path: file)}
                _ -> {:file, Build.Warp.FileRequirement.new(path: hrl)}
              end
        end

      Build.Warp.Requirement.new(requirement: req)
    end)
  end

  defp handle_modules(modules) do
    modules
    |> Enum.uniq()
    |> Enum.reject(fn mod -> Deps.is_standard_module?(mod) end)
    |> Enum.map(fn dep ->
      dep = Atom.to_string(dep)

      glob = Path.wildcard("./**/#{dep}.erl")
      Logger.info("Found module #{dep} in #{glob}")

      req =
        case glob do
          [file | _] -> {:file, Build.Warp.FileRequirement.new(path: file)}
          _ -> {:symbol, Build.Warp.SymbolRequirement.new(raw: dep, kind: "module")}
        end

      Build.Warp.Requirement.new(requirement: req)
    end)
  end

  def clean_warp_store_path(path) do
    case Path.split(path) do
      [_warp, _store, _hash | path] -> Path.join(path)
      _ -> path
    end
  end
end
