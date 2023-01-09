defmodule Analyzer.GenerateSignature do
  require Logger

  def generate_signature(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    cond do
      Path.extname(req.file) in [".erl", ".hrl"] ->
        do_gen_sig_erl(req)

      true ->
        Build.Warp.Codedb.GenerateSignatureResponse.new(status: :STATUS_ERR)
    end
  end

  defp do_gen_sig_erl(req) do
    include_paths =
      Analyzer.ErlangHacks.IncludePaths.from_file_and_deps(req.file, req.dependencies)

    code_paths =
      for dep <- req.dependencies do
        dep.store_path |> Path.dirname()
      end
      |> Enum.uniq()

    IO.inspect(["Adding code paths: ", code_paths])

    {:ok, result} =
      :erl_generate_signature.generate(%{
        file: req.file,
        include_paths: include_paths,
        code_paths: code_paths
      })

    IO.inspect(result)

    response = handle_result(req, result)

    Build.Warp.Codedb.GenerateSignatureResponse.new(response: response)
    |> IO.inspect()
  end

  defp handle_result(req, {:missing_dependencies, deps}) do
    includes =
      (deps[:includes] || [])
      |> Enum.uniq()
      |> Enum.map(fn dep ->
        req = {:file, Build.Warp.FileRequirement.new(path: dep)}
        Build.Warp.Requirement.new(requirement: req)
      end)

    parse_transforms =
      (deps[:parse_transforms] || [])
      |> Enum.uniq()
      |> Enum.map(fn dep ->
        dep = Atom.to_string(dep)
        req = {:symbol, Build.Warp.SymbolRequirement.new(raw: dep, kind: "module")}
        Build.Warp.Requirement.new(requirement: req)
      end)

    type_modules =
      (deps[:type_modules] || [])
      |> Enum.uniq()
      |> Enum.map(fn dep ->
        dep = Atom.to_string(dep)
        req = {:symbol, Build.Warp.SymbolRequirement.new(raw: dep, kind: "module")}
        Build.Warp.Requirement.new(requirement: req)
      end)

    resp =
      Build.Warp.Codedb.GetAstMissingDepsResponse.new(
        file: req.file,
        symbol: req.symbol,
        dependencies: includes ++ parse_transforms ++ type_modules
      )

    {:missing_deps, resp}
  end

  defp handle_result(req, {:completed, signatures}) do
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
end
