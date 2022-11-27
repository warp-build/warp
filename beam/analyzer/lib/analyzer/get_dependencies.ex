defmodule Analyzer.GetDependencies do
  require Logger

  def get_dependencies(req, _stream) do
    with {:ok, rebar_deps} <- do_get_rebar_config_deps(req),
         {:ok, lock_deps} <- do_get_rebar_lock_deps(req),
         {:ok, erlmk_deps} <- do_get_erlangmk_deps(req) do
      dependencies = rebar_deps ++ lock_deps ++ erlmk_deps

      for dep <- dependencies do
        Logger.debug("Found dep: #{inspect(dep)}")
      end

      Build.Warp.Codedb.GetDependenciesResponse.new(dependencies: dependencies)
    else
      e ->
        Logger.error("Error finding dependencies: #{inspect(e)}")
        Build.Warp.Codedb.GetDependenciesResponse.new(dependencies: [])
    end
  end

  defp do_get_erlangmk_deps(req) do
    with {:ok, file} <- File.read(Path.join(req.workspace_root, "Makefile")) do
      lines = String.split(file, <<"\n">>)

      deps =
        lines
        |> Enum.map(&String.split(&1, " "))
        |> Enum.reduce([], fn
          # NOTE(@ostera): erlang.mk is its own dependeny so we gotta skip this
          [<<"dep_ci.erlang.mk">> | _], acc ->
            acc

          [<<"dep_", name::binary>>, "=", "git", repo, version], acc ->
            [
              Build.Warp.Dependency.new(
                url: String.replace(repo, ".git", ""),
                name: name,
                version: version,
                signature_resolver: "https://tools.warp.build/hexpm/resolver",
                archive_resolver:
                  if String.contains?(repo, "github") do
                    "https://tools.warp.build/github/resolver"
                  else
                    "https://tools.warp.build/gitlab/resolver"
                  end
              )
              | acc
            ]

          _line, acc ->
            acc
        end)

      {:ok, deps}
    else
      e ->
        Logger.error("Error reading erlang.mk deps: #{inspect(e)}")
        {:ok, []}
    end
  end

  defp do_get_rebar_config_deps(req) do
    with {:ok, config} <- :file.consult(Path.join(req.workspace_root, "rebar.config")) do
      deps = Keyword.get(config, :deps, [])

      test_deps =
        if "test" in req.profiles do
          config |> Keyword.get(:profiles, []) |> Keyword.get(:test, []) |> Keyword.get(:deps, [])
        else
          []
        end

      deps = deps ++ test_deps

      hex_config =
        :hex_core.default_config()
        |> Map.merge(%{
          http_user_agent_fragment: "(tools.warp.build/beam/analyzer)",
          http_adapter: {Analyzer.HexHttp, %{}}
        })

      dependencies =
        for dep <- deps do
          dep =
            case dep do
              {name, version, {:pkg, pkg_name}} ->
                Build.Warp.Dependency.new(
                  url: "https://hex.pm/packages/#{pkg_name |> Atom.to_string()}",
                  name: name |> Atom.to_string(),
                  version: version |> :binary.list_to_bin(),
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver: "https://tools.warp.build/hexpm/resolver"
                )

              {name, _version, {:git, repo, ref}} ->
                ref =
                  case ref do
                    {:ref, ref} -> ref
                    {:branch, branch} -> branch
                    {:tag, tag} -> tag
                    _ -> ref
                  end

                ref =
                  cond do
                    is_atom(ref) -> Atom.to_string(ref)
                    true -> :binary.list_to_bin(ref)
                  end

                repo = :binary.list_to_bin(repo)

                Build.Warp.Dependency.new(
                  url: String.replace(repo, ".git", ""),
                  name: name |> Atom.to_string(),
                  version: ref,
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver:
                    if String.contains?(repo, "github") do
                      "https://tools.warp.build/github/resolver"
                    else
                      "https://tools.warp.build/gitlab/resolver"
                    end
                )

              {name, {:git, repo, ref}} ->
                ref =
                  case ref do
                    {:ref, ref} -> ref
                    {:branch, branch} -> branch
                    {:tag, tag} -> tag
                  end

                ref =
                  cond do
                    is_atom(ref) -> Atom.to_string(ref)
                    true -> :binary.list_to_bin(ref)
                  end

                repo = :binary.list_to_bin(repo)

                Build.Warp.Dependency.new(
                  url: String.replace(repo, ".git", ""),
                  name: name |> Atom.to_string(),
                  version: ref,
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver:
                    if String.contains?(repo, "github") do
                      "https://tools.warp.build/github/resolver"
                    else
                      "https://tools.warp.build/gitlab/resolver"
                    end
                )

              {name, {:git_subdir, repo, ref, subdir}} ->
                ref =
                  case ref do
                    {:ref, ref} -> ref
                    {:branch, branch} -> branch
                    {:tag, tag} -> tag
                  end

                ref =
                  cond do
                    is_atom(ref) -> Atom.to_string(ref)
                    true -> :binary.list_to_bin(ref)
                  end

                repo = :binary.list_to_bin(repo)

                Build.Warp.Dependency.new(
                  url: String.replace(repo, ".git", ""),
                  name: name |> Atom.to_string(),
                  version: ref,
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_subdir: subdir |> :binary.list_to_bin(),
                  archive_resolver:
                    if String.contains?(repo, "github") do
                      "https://tools.warp.build/github/resolver"
                    else
                      "https://tools.warp.build/gitlab/resolver"
                    end
                )

              {name, version} ->
                Build.Warp.Dependency.new(
                  url: "https://hex.pm/packages/#{name |> Atom.to_string()}",
                  name: name |> Atom.to_string(),
                  version: version |> :binary.list_to_bin(),
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver: "https://tools.warp.build/hexpm/resolver"
                )

              name when is_atom(name) ->
                {:ok, {_, _, resp}} = :hex_api_package.get(hex_config, Atom.to_string(name))

                Build.Warp.Dependency.new(
                  url: "https://hex.pm/packages/#{name |> Atom.to_string()}",
                  name: name |> Atom.to_string(),
                  version: resp["latest_stable_version"] || resp["latest_version"],
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver: "https://tools.warp.build/hexpm/resolver"
                )
            end

          dep
        end

      {:ok, dependencies}
    else
      e ->
        Logger.error("Error reading rebar3 deps: #{inspect(e)}")
        {:ok, []}
    end
  end

  defp do_get_rebar_lock_deps(req) do
    with {:ok, [{_, locked_deps} | _]} <-
           :file.consult(Path.join(req.workspace_root, "rebar.lock")) do
      dependencies =
        for {name, spec, _} <- locked_deps do
          dep =
            case spec do
              {:pkg, pkg_name, version} ->
                Build.Warp.Dependency.new(
                  url: "https://hex.pm/packages/#{pkg_name}",
                  name: name,
                  version: version,
                  signature_resolver: "https://tools.warp.build/hexpm/resolver",
                  archive_resolver: "https://tools.warp.build/hexpm/resolver"
                )

              {:git, repo, ref} ->
                ref =
                  case ref do
                    {:ref, ref} -> ref
                    {:branch, branch} -> branch
                    {:tag, tag} -> tag
                  end

                repo = :binary.list_to_bin(repo)

                Build.Warp.Dependency.new(
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

          dep
        end

      {:ok, dependencies}
    else
      e ->
        Logger.error("Error reading rebar.lock deps: #{inspect(e)}")
        {:ok, []}
    end
  end
end
