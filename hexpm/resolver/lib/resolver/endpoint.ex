defmodule Resolver.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Logger.Server
  run Resolver.Server
end
