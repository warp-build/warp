defmodule Tricorder.Grpc.Ops.GenerateSignature do
  require Logger

  alias Tricorder.Analysis
  alias Tricorder.Deps

  def generate_signature(req, stream) do
    Logger.info("Analyzing: #{req.file}")

    with {:ok, response} <- do_generate_signature(req) do
      Logger.info("Response: #{inspect(response)}")
      Build.Warp.Tricorder.GenerateSignatureResponse.new(response: response)
    else
      err -> Logger.error("#{err}")
    end
  end

  defp do_generate_signature(req) do
    paths =
      Analysis.Paths.build_paths(
        req.file,
        Enum.map(req.dependencies, fn req -> req.store_path end)
      )

    cond do
      Path.basename(req.file) in ["mix.exs"] ->
        {:ok, analysis} = Analysis.Mix.analyze(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      Path.basename(req.file) in ["rebar.config"] ->
        {:ok, analysis} = Analysis.Rebar3.analyze(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      Path.extname(req.file) in [".erl", ".hrl"] ->
        {:ok, analysis} = Analysis.Erlang.analyze(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      Path.extname(req.file) in [".ex", ".exs"] ->
        {:ok, analysis} = Analysis.Elixir.analyze_script(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      Path.extname(req.file) in [".ex"] ->
        {:ok, analysis} = Analysis.Elixir.analyze_lib(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      true ->
        {:error, :unhandled_input}
    end
  end

  defp analysis_to_resp(req, {:missing_dependencies, deps}) do
    includes = Map.get(deps, :includes, []) |> handle_includes

    modules = Map.get(deps, :modules, []) |> handle_modules

    resp =
      Build.Warp.Tricorder.GenerateSignatureMissingDepsResponse.new(
        file: req.file,
        symbol: req.symbol,
        requirements: includes ++ modules
      )

    {:missing_deps, resp}
  end

  defp analysis_to_resp(req, {:completed, signatures}) do
    signatures =
      signatures
      |> Enum.map(fn sig ->
        modules = Map.get(sig, :modules, []) |> handle_modules

        type_modules = Map.get(sig, :type_modules, []) |> handle_modules

        parse_transforms =
          Map.get(sig, :parse_transforms, [])
          |> handle_modules

        includes = Map.get(sig, :includes, []) |> handle_includes

        {:ok, config} =
          sig
          |> Map.delete(:deps)
          |> Map.delete(:runtime_deps)
          |> Map.delete(:name)
          |> Map.delete(:rule)
          |> Jason.encode!()
          |> Jason.decode!()
          |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

        test_lib = Map.get(sig, :test, nil)

        test_lib =
          cond do
            test_lib != nil ->
              symbol =
                Build.Warp.SymbolRequirement.new(
                  raw: Path.basename(test_lib, ".erl"),
                  kind: "module"
                )

              [Build.Warp.Requirement.new(requirement: {:symbol, symbol})] ++
                modules

            true ->
              []
          end

        deps =
          includes ++
            parse_transforms ++
            type_modules ++
            test_lib

        Build.Warp.Signature.new(
          name: sig.name,
          rule: sig.rule,
          deps: deps,
          runtime_deps: modules,
          config: config
        )
      end)

    {:ok,
     Build.Warp.Tricorder.GenerateSignatureSuccessResponse.new(
       file: req.file,
       signatures: signatures
     )}
  end

  defp handle_includes(includes) do
    includes
    |> Enum.uniq()
    |> Enum.map(fn hrl ->
      req =
        case Deps.find_header(hrl) do
          {:ok, dep} ->
            {:url, Build.Warp.UrlRequirement.new(url: dep.url)}

          _ ->
            {:file, Build.Warp.FileRequirement.new(path: hrl)}
        end

      Build.Warp.Requirement.new(requirement: req)
    end)
  end

  defp handle_modules(modules) do
    modules
    |> Enum.uniq()
    |> Enum.map(fn dep ->
      dep = Atom.to_string(dep)
      req = {:symbol, Build.Warp.SymbolRequirement.new(raw: dep, kind: "module")}
      Build.Warp.Requirement.new(requirement: req)
    end)
  end
end
