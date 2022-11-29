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
    parts = req.file |> Path.dirname() |> Path.split() |> Enum.drop(1)

    include_paths =
      [
        Enum.map(req.dependencies, fn dep ->
          Path.dirname(Path.dirname(dep.store_path))
        end),
        for i <- 1..(Enum.count(parts) - 1) do
          path =
            case Enum.take(parts, i + 1) do
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

        includes =
          sig.includes
          |> Enum.map(fn dep ->
            dep =
              case String.split(dep, "/") do
                [_app, include, file] -> Path.join(include, file)
                _ -> dep
              end

            req = {:file, Build.Warp.FileRequirement.new(path: dep)}

            Build.Warp.Requirement.new(requirement: req)
          end)

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
          deps: includes,
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
