defmodule Tricorder.Grpc.Ops.PrepareDependency do
  require Logger

  def prepare_dependency(req, _stream) do
    root = req.package_root

    cond do
      Path.join(root, "metadata.config") |> File.exists?() ->
        prepare_hex_workspace(req)

      Path.join(root, "rebar.config") |> File.exists?() ||
          Path.join(root, "rebar.lock") |> File.exists?() ->
        prepare_rebar3_workspace(req)

      Path.join(root, "Makefile") |> File.exists?() ->
        prepare_erlangmk_workspace(req)

      true ->
        {:ok, config} =
          %{srcs: Path.wildcard("#{root}/**/*")}
          |> Jason.encode!()
          |> Jason.decode!()
          |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

        signature =
          Build.Warp.Signature.new(
            name: req.package_name,
            rule: "files",
            deps: [],
            config: config,
            runtime_deps: []
          )

        Build.Warp.Tricorder.PrepareDependencyResponse.new(
          status: :STATUS_OK,
          signatures: [signature]
        )
    end
  end

  defp prepare_erlangmk_workspace(req) do
    root = req.package_root

    {:ok, file} = File.read(Path.join(root, "Makefile"))
    lines = String.split(file, <<"\n">>)

    tokens =
      lines
      |> Enum.map(&String.split(&1, " "))
      |> Enum.reject(fn
        [] -> true
        [""] -> true
        ["#" | _] -> true
        _ -> false
      end)

    ["PROJECT", "=", name | _] =
      tokens
      |> Enum.find(fn
        ["PROJECT", "=", name | _] -> true
        _ -> false
      end)

    deps =
      tokens
      |> Enum.reduce([], fn
        # NOTE(@ostera): erlang.mk is its own dependeny so we gotta skip this
        [<<"dep_ci.erlang.mk">> | _], acc ->
          acc

        [<<"dep_", _name::binary>>, "=", "git", repo, _version], acc ->
          url = Build.Warp.UrlRequirement.new(url: String.replace(repo, ".git", ""))
          req = Build.Warp.Requirement.new(requirement: {:url, url})

          [req | acc]

        _line, acc ->
          acc
      end)

    {:ok, config} =
      %{
        srcs:
          [
            Path.wildcard("#{root}/src/*"),
            Path.wildcard("#{root}/include/*"),
            Path.wildcard("#{root}/c_src/*"),
            "#{root}/Makefile",
            "#{root}/erlang.mk"
          ]
          |> List.flatten()
      }
      |> Jason.encode!()
      |> Jason.decode!()
      |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

    signature =
      Build.Warp.Signature.new(
        name: name,
        rule: "erlangmk_library",
        deps: deps,
        config: config,
        runtime_deps: []
      )

    Build.Warp.Tricorder.PrepareDependencyResponse.new(
      status: :STATUS_OK,
      signatures: [signature]
    )
  end

  defp prepare_hex_workspace(req) do
    root = req.package_root

    metadata = read_metadata_config(root)

    name = :maps.get("app", metadata)

    rule = get_rule(metadata)

    deps =
      get_deps(metadata)
      |> Enum.map(fn dep ->
        url = Build.Warp.UrlRequirement.new(url: dep)
        Build.Warp.Requirement.new(requirement: {:url, url})
      end)

    {:ok, config} =
      %{srcs: get_sources(metadata, root, name)}
      |> Jason.encode!()
      |> Jason.decode!()
      |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

    signature =
      Build.Warp.Signature.new(
        name: name,
        rule: rule,
        deps: deps,
        config: config,
        runtime_deps: []
      )

    Build.Warp.Tricorder.PrepareDependencyResponse.new(
      status: :STATUS_OK,
      signatures: [signature]
    )
  end

  def read_metadata_config(root) do
    metadata_config = Path.join(root, "metadata.config")
    {:ok, metadata} = :file.consult(metadata_config)
    :proplists.to_map(metadata)
  end

  def get_sources(metadata, root, package_name) do
    source_root = Path.join(root, package_name)

    contents_tar = Path.join(root, "contents.tar.gz")
    :ok = :erl_tar.extract(contents_tar, [:compressed, {:cwd, source_root}])

    :maps.get("files", metadata, [])
    |> Enum.map(fn
      {path, _src} -> path
      path -> path
    end)
    |> Enum.filter(fn path ->
      source_path = Path.join(source_root, path)
      File.exists?(source_path) && not File.dir?(source_path)
    end)
  end

  def get_rule(metadata) do
    tools = :maps.get("build_tools", metadata, [])

    cond do
      "rebar3" in tools -> "rebar3_library"
      "rebar" in tools -> "rebar3_library"
      "mix" in tools -> "mix_library"
      "make" in tools -> "erlangmk_library"
      true -> "mix_library"
    end
  end

  def get_deps(metadata) do
    for req <- Map.get(metadata, "requirements", []) do
      case req do
        {name, spec} ->
          spec = Map.new(spec)
          "https://hex.pm/packages/#{name}"

        spec when is_list(spec) ->
          spec = Map.new(spec)
          "https://hex.pm/packages/#{spec["name"]}"
      end
    end
  end

  defp prepare_rebar3_workspace(req) do
    rebar_config = Path.join(req.package_root, "rebar.config")

    deps =
      with {:ok, config} <- :file.consult(rebar_config) do
        Keyword.get(config, :deps, [])
        |> Enum.map(fn dep ->
          url =
            case dep do
              {name, version, {:pkg, pkg_name}} ->
                Build.Warp.UrlRequirement.new(
                  url: "https://hex.pm/packages/#{pkg_name |> Atom.to_string()}"
                )

              {name, _version, {:git, repo, ref}} ->
                repo = :binary.list_to_bin(repo)
                Build.Warp.UrlRequirement.new(url: String.replace(repo, ".git", ""))

              {name, {:git, repo, ref}} ->
                repo = :binary.list_to_bin(repo)
                Build.Warp.UrlRequirement.new(url: String.replace(repo, ".git", ""))

              {name, version} ->
                Build.Warp.UrlRequirement.new(
                  url: "https://hex.pm/packages/#{name |> Atom.to_string()}"
                )

              name when is_atom(name) ->
                Build.Warp.DependencyRequirement.new(
                  url: "https://hex.pm/packages/#{name |> Atom.to_string()}"
                )
            end

          Build.Warp.Requirement.new(requirement: {:url, url})
        end)
      else
        _ -> []
      end

    {:ok, config} =
      %{}
      |> Jason.encode!()
      |> Jason.decode!()
      |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

    signature =
      Build.Warp.Signature.new(
        name: req.package_name,
        rule: "rebar3_library",
        deps: deps,
        config: config,
        runtime_deps: []
      )

    Build.Warp.Tricorder.PrepareDependencyResponse.new(
      status: :STATUS_OK,
      signatures: [signature]
    )
  end
end
