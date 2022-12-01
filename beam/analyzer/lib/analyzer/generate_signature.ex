defmodule Analyzer.GenerateSignature do
  require Logger

  def generate_signature(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    cond do
      Path.extname(req.file) in [".erl", ".hrl"] ->
        gen_sig_erl(req)

      true ->
        Build.Warp.Codedb.GenerateSignatureResponse.new(status: :STATUS_ERR)
    end
  end

  defp gen_sig_erl(req) do
    # FIXME(@ostera): this is a hack to make the source analyzer find headers
    #

    # NOTE(@ostera): we don't want to discover things on the FS root
    parts =
      case req.file |> Path.dirname() |> Path.split() do
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

    Logger.info("generating signature using include paths:")

    for p <- include_paths do
      IO.inspect(p)
    end

    signatures =
      :source_analyzer.analyze_one(
        req.file,
        _ModMap = %{},
        _IgnoreModMap = %{},
        include_paths
      )

    signatures = :maps.get(:signatures, signatures, [])

    gen_sig =
      %{
        :version => 0,
        :signatures => signatures
      }
      |> Jason.encode!()

    IO.inspect(signatures)

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

        Build.Warp.Signature.new(
          name: sig.name,
          rule: sig.rule,
          deps: includes ++ parse_transforms ++ type_modules,
          runtime_deps: modules,
          config: config
        )
      end)

    Build.Warp.Codedb.GenerateSignatureResponse.new(
      status: :STATUS_OK,
      file: req.file,
      json_signature: gen_sig,
      signatures: signatures
    )
  end
end
