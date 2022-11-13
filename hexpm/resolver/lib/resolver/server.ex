defmodule Resolver.Server do
  use GRPC.Server, service: Build.Warp.Dependency.ResolverService.Service

  require Logger

  @doc """

  Resolve a single dependency by package name and version.

  """
  def resolve_dependency(req, _stream) do
    config =
      :hex_core.default_config()
      |> Map.merge(%{
        http_user_agent_fragment: "(tools.warp.build/hexpm/resolver)",
        http_adapter: {Resolver.HexHttp, %{}}
      })

    {:ok, resp} = :hex_api_release.get(config, req.package_name, req.version)

    case resp do
      {200, _meta, pkg} ->
        archive =
          Build.Warp.Archive.new(
            url: "https://repo.hex.pm/tarballs/#{req.package_name}-#{pkg["version"]}.tar",
            sha256: pkg["checksum"]
          )

        Build.Warp.Dependency.ResolveDependencyResponse.new(
          status: :STATUS_OK,
          package_name: req.package_name,
          version: pkg["version"],
          archive: archive
        )

      _ ->
        Build.Warp.Dependency.ResolveDependencyResponse.new(status: :STATUS_ERR)
    end
  end

  def prepare_dependency(req, _stream) do
    root = req.package_root

    cond do
      Path.join(root, "metadata.config") |> File.exists?() ->
        prepare_hex_workspace(req)

      Path.join(root, "rebar.config") |> File.exists?() ->
        prepare_rebar3_workspace(req)

      true ->
        Build.Warp.Dependency.PrepareDependencyResponse.new(status: :STATUS_ERR)
    end
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

    Build.Warp.Dependency.PrepareDependencyResponse.new(
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
      "mix" in tools -> "mix_library"
      "make" in tools -> "erlangmk_library"
      true -> "mix_library"
    end
  end

  def get_deps(metadata) do
    for req <- requirements(metadata) do
      name = :proplists.to_map(req) |> Map.get("name")
      "https://hex.pm/packages/#{name}"
    end
  end

  def requirements(metadata) do
    reqs =
      case :maps.get("requirements", metadata, []) do
        x when is_map(x) -> :maps.to_list(x)
        y -> y
      end

    :lists.map(
      fn
        {name, req} -> [{"name", name} | req]
        req when is_list(req) -> req
      end,
      reqs
    )
  end

  defp prepare_rebar3_workspace(req) do
    metadata = read_rebar_config(req.package_root)

    deps =
      metadata.deps
      |> Enum.map(fn dep ->
        url = Build.Warp.UrlRequirement.new(url: dep)
        Build.Warp.Requirement.new(requirement: {:url, url})
      end)

    config =
      %{}
      |> Jason.encode!()
      |> Jason.decode!()
      |> Protobuf.JSON.from_decoded(Google.Protobuf.Struct)

    signature =
      Build.Warp.Signature.new(
        name: ":#{req.package_name}",
        rule: "rebar3_library",
        deps: deps,
        config: config,
        runtime_deps: []
      )

    Build.Warp.Dependency.PrepareDependencyResponse.new(
      status: :STATUS_OK,
      signatures: [signature]
    )
  end

  def read_rebar_config(root) do
    metadata_config = Path.join(root, "rebar.config")
    {:ok, metadata} = :file.consult(metadata_config)
    :maps.from_list(metadata)
  end
end
