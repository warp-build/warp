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
    signatures =
      :source_analyzer.analyze_one(
        req.file,
        _ModMap = %{},
        _IgnoreModMap = %{},
        _IncludePaths = []
      )

    signatures = :maps.get(:signatures, signatures, [])


    gen_sig =
      %{
        :version => 0,
        :signatures => signatures
      }
      |> Jason.encode!()

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
            req =
              if String.contains?(dep, "/include/") do
                [app, _include, _file] = String.split(dep, "/")

                dep =
                  Build.Warp.DependencyRequirement.new(
                    name: app,
                    url: "https://hex.pm/packages/#{app}"
                  )

                {:dependency, dep}
              else
                {:file, Build.Warp.FileRequirement.new(path: dep)}
              end

            Build.Warp.Requirement.new(requirement: req)
          end)

        deps = modules ++ includes

        runtime_deps =
          sig.runtime_deps
          |> Enum.map(fn dep ->
            symbol =
              Build.Warp.SymbolRequirement.new(
                raw: dep,
                kind: "module"
              )

            Build.Warp.Requirement.new(requirement: {:symbol, symbol})
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
          deps: deps,
          runtime_deps: runtime_deps,
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
