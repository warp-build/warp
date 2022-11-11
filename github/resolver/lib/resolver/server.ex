defmodule Resolver.Server do
  use GRPC.Server, service: Build.Warp.Dependency.ResolverService.Service

  require Logger

  def resolve_dependency(req, _stream) do
    with %URI{ host: "github.com" } <- URI.parse(req.url) do
      [_scheme, _, _github, _username, repo] = String.split(req.url, "/")

      archive = Build.Warp.Archive.new(
        url: "#{req.url}/archive/#{req.version}.tar.gz",
        strip_prefix: "#{repo}-#{req.version}"
      )

      Build.Warp.Dependency.ResolveDependencyResponse.new(
        status: :STATUS_OK,
        package_name: req.package_name,
        version: req.version,
        archive: archive
      )
    else
      err -> Build.Warp.Dependency.ResolveDependencyResponse.new(
        status: :STATUS_ERR
      )
    end
  end
end
