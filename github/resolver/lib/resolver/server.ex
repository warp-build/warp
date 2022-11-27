defmodule Resolver.Server do
  use GRPC.Server, service: Build.Warp.Dependency.ResolverService.Service

  require Logger

  def resolve_dependency(req, _stream) do
    with url = %URI{host: "github.com"} <- URI.parse(req.url) do
      [_scheme, _, _github, _username, repo] = String.split(req.url, "/")

      url =
        case url do
          %URI{scheme: "git"} -> %URI{url | scheme: "https", port: 443}
          _ -> url
        end

      version =
        case req.version do
          <<"v", _::binary>> -> req.version |> String.replace("v", "")
          _ -> req.version
        end

      archive =
        Build.Warp.Archive.new(
          url: "#{URI.to_string(url)}/archive/#{req.version}.tar.gz",
          strip_prefix: "#{repo}-#{version}"
        )

      Build.Warp.Dependency.ResolveDependencyResponse.new(
        status: :STATUS_OK,
        package_name: req.package_name,
        version: req.version,
        archive: archive
      )
    else
      err -> Build.Warp.Dependency.ResolveDependencyResponse.new(status: :STATUS_ERR)
    end
  end
end
