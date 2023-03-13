defmodule Tricorder.Grpc.Ops.GenerateSignature do
  require Logger

  alias Tricorder.Analysis

  def generate_signature(req, stream) do
    Logger.info("Analyzing: #{req.file}")

    with {:ok, response} <- do_generate_signature(req) do
      Build.Warp.Tricorder.GenerateSignatureResponse.new(response: response)
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
        {:ok, mix_analysis_to_resp(req, analysis)}

      Path.extname(req.file) in [".erl", ".hrl"] ->
        {:ok, analysis} = Analysis.Erlang.analyze(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      Path.extname(req.file) in [".ex", ".exs"] ->
        {:ok, analysis} = Analysis.Elixir.analyze(req.file, paths)
        {:ok, analysis_to_resp(req, analysis)}

      true ->
        {:error, :unhandled_input}
    end
  end

  defp analysis_to_resp(req, {:missing_dependencies, deps}) do
    includes =
      (deps[:includes] || [])
      |> Enum.uniq()
      |> Enum.map(fn dep ->
        req = {:file, Build.Warp.FileRequirement.new(path: dep)}
        Build.Warp.Requirement.new(requirement: req)
      end)

    modules =
      (deps[:modules] || [])
      |> Enum.uniq()
      |> Enum.map(fn dep ->
        dep = Atom.to_string(dep)
        req = {:symbol, Build.Warp.SymbolRequirement.new(raw: dep, kind: "module")}
        Build.Warp.Requirement.new(requirement: req)
      end)

    resp =
      Build.Warp.Codedb.GenerateSignatureMissingDepsResponse.new(
        file: req.file,
        symbol: req.symbol,
        dependencies: includes ++ modules
      )

    {:missing_deps, resp}
  end

  defp analysis_to_resp(req, {:completed, signatures}) do
    signatures =
      signatures
      |> Enum.map(fn sig ->
        modules =
          sig.modules
          |> Enum.map(fn dep ->
            symbol =
              Build.Warp.SymbolRequirement.new(
                raw: Atom.to_string(dep),
                kind: "module"
              )

            Build.Warp.Requirement.new(requirement: {:symbol, symbol})
          end)
          |> Enum.uniq()

        type_modules =
          Map.get(sig, :type_modules, [])
          |> Enum.map(fn dep ->
            symbol =
              Build.Warp.SymbolRequirement.new(
                raw: Atom.to_string(dep),
                kind: "module"
              )

            Build.Warp.Requirement.new(requirement: {:symbol, symbol})
          end)
          |> Enum.uniq()

        parse_transforms =
          Map.get(sig, :parse_transforms, [])
          |> Enum.map(fn dep ->
            symbol =
              Build.Warp.SymbolRequirement.new(
                raw: Atom.to_string(dep),
                kind: "module"
              )

            Build.Warp.Requirement.new(requirement: {:symbol, symbol})
          end)
          |> Enum.uniq()

        includes =
          sig.includes
          |> Enum.map(fn dep ->
            req = {:file, Build.Warp.FileRequirement.new(path: dep)}
            Build.Warp.Requirement.new(requirement: req)
          end)
          |> Enum.uniq()

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
              {:ok, test_mod} = :erl_stdlib.file_to_module(test_lib)

              symbol =
                Build.Warp.SymbolRequirement.new(
                  raw: Atom.to_string(test_mod),
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
     Build.Warp.Codedb.GenerateSignatureSuccessResponse.new(
       file: req.file,
       signatures: signatures
     )}
  end

  defp mix_analysis_to_resp(req, {:escript, bin}) do
    {:ok, config} =
      %{
        bin: bin
      }
      |> Jason.encode!()
      |> Jason.decode!()
      |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

    signatures = [
      Build.Warp.Signature.new(
        name: req.file,
        rule: "mix_escript",
        deps: [],
        runtime_deps: [],
        config: config
      )
    ]

    {:ok,
     Build.Warp.Codedb.GenerateSignatureSuccessResponse.new(
       file: req.file,
       signatures: signatures
     )}
  end

  defp mix_analysis_to_resp(req, {:missing_dependencies, deps}) do
  end
end
