defmodule Analyzer.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  run(Analyzer.Server)
end
