defmodule Analyzer.Server do
  use GRPC.Server, service: Build.Warp.Codedb.AnalyzerService.Service

  require Logger

  def get_interested_paths(_request, _stream) do
    Build.Warp.Codedb.GetInterestedPathsResponse.new(
      build_files: [
        ".eex",
        ".erl",
        ".ex",
        ".exs",
        ".hrl",
        "rebar.config",
        "rebar.lock",
        "mix.lock",
        "mix.exs"
      ],
      test_files: ["*_SUITE.erl", "prop_*.erl"]
    )
  end

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
                dep = Build.Warp.DependencyRequirement.new(
                  name: app,
                  url: "https://hex.pm/packages/#{app}"
                )
                {:dependency, dep}
              else
                {:symbol, Build.Warp.FileRequirement.new(path: dep)}
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

    IO.inspect(signatures)

    Build.Warp.Codedb.GenerateSignatureResponse.new(
      status: :STATUS_OK,
      file: req.file,
      json_signature: gen_sig,
      signatures: signatures
    )
  end

  def get_ast(req, _stream) do
    Logger.info("Analyzing: #{req.file}")

    cond do
      Path.extname(req.file) in [".erl", ".hrl"] -> do_get_erl_ast(req)
    end
  end

  defp do_get_erl_ast(req) do
    file = req.file
    {:ok, %{^file => result}} = :erl_analyzer.analyze([file], _ModMap = %{}, _IncludePaths = [])

    symbol =
      case req.symbol.sym do
        {:all, true} ->
          :all

        {:named, sym} ->
          [f, a] =
            case sym |> String.split("/") do
              [sym] -> [sym, "0"]
              fa -> fa
            end

          {:named, {String.to_atom(f), String.to_integer(a)}}
      end

    {:ok, ast} = :erl_analyzer.subtree(result, symbol)

    source =
      ast
      |> :erl_syntax.form_list()
      |> :erl_prettypr.format(encoding: :utf8)
      |> :binary.list_to_bin()

    Build.Warp.Codedb.GetAstResponse.new(
      status: :STATUS_OK,
      file: file,
      symbol: Build.Warp.Symbol.new(sym: req.symbol.sym),
      ast: Kernel.inspect(ast),
      source: source
    )
  end

  defp get_provided_symbols(req, _stream) do
    cond do
      Path.extname(req.file) in [".erl", ".hrl"] ->
        do_get_provided_symbols(req)

      Path.basename(req.file) in ["rebar.lock"] ->
        do_get_rebar_lock_deps(req)

      true ->
        Logger.info("Skipped #{req.file}")
        Build.Warp.Codedb.GetProvidedSymbolsResponse.new(skipped: true, file: req.file)
    end
  end

  defp do_get_rebar_lock_deps(req) do
    with {:ok, [{_, locked_deps} | _]} <- :file.consult(req.file) do
      provides =
        for {name, spec, _} <- locked_deps do
          dep_req =
            case spec do
              {:pkg, pkg_name, version} ->
                Build.Warp.DependencyRequirement.new(
                  url: "https://hex.pm/packages/#{pkg_name}",
                  name: name,
                  version: version,
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver: "https://tools.warp.build/hexpm/resolver"
                )

              {:git, repo, ref} ->
                ref = case ref do
                  {:ref, ref} -> ref
                  {:branch, branch} -> branch
                  {:tag, tag} -> tag
                end

                repo = :binary.list_to_bin(repo)

                Build.Warp.DependencyRequirement.new(
                  url: String.replace(repo, ".git", ""),
                  name: name,
                  version: ref |> :binary.list_to_bin(),
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver:
                    if String.contains?(repo, "github") do
                      "https://tools.warp.build/github/resolver"
                    else
                      "https://tools.warp.build/gitlab/resolver"
                    end
                )
            end

          IO.inspect(dep_req)

          Build.Warp.Requirement.new(requirement: {:dependency, dep_req})
        end

      Build.Warp.Codedb.GetProvidedSymbolsResponse.new(
        skipped: false,
        file: req.file,
        provides: provides
      )
    else
      _ ->
      Logger.info("Could not find dependencies in lock file")
        Build.Warp.Codedb.GetProvidedSymbolsResponse.new(
          skipped: false,
          file: req.file,
          provides: []
        )
    end
  end

  defp do_get_provided_symbols(req) do
    Logger.info("Analyzing: #{req.file}")

    file = req.file
    {:ok, %{^file => result}} = :erl_analyzer.analyze([file], _ModMap = %{}, _IncludePaths = [])

    requirement =
      case Path.extname(req.file) do
        ".erl" ->
          {:ok, mod_name} = :erl_stdlib.file_to_module(req.file)
          mod_name = mod_name |> Atom.to_string()
          {:symbol, Build.Warp.SymbolRequirement.new(raw: mod_name, kind: "module")}

        ".hrl" ->
          {:file, Build.Warp.FileRequirement.new(path: req.file)}
      end

    exported_fns =
      :erl_analyzer.exported_functions(result)
      |> Enum.map(fn {m, f, a} ->
        symbol =
          Build.Warp.SymbolRequirement.new(
            raw: "#{Atom.to_string(m)}:#{Atom.to_string(f)}/#{a}",
            kind: "function"
          )

        Build.Warp.Requirement.new(requirement: {:symbol, symbol})
      end)

    exported_types =
      :erl_analyzer.exported_types(result)
      |> Enum.map(fn {m, f, a} ->
        symbol =
          Build.Warp.SymbolRequirement.new(
            raw: "#{Atom.to_string(m)}:#{Atom.to_string(f)}/#{a}",
            kind: "type"
          )

        Build.Warp.Requirement.new(requirement: {:symbol, symbol})
      end)

    provides =
      [Build.Warp.Requirement.new(requirement: requirement)] ++
        exported_fns ++
        exported_types

    Build.Warp.Codedb.GetProvidedSymbolsResponse.new(
      skipped: false,
      file: req.file,
      provides: provides
    )
  end
end
