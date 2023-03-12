defmodule Tricorder.Grpc.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  run(Tricorder.Grpc.Server)
end

