defmodule Tricorder.Grpc.Server do
  use GRPC.Server, service: Build.Warp.Tricorder.TricorderService.Service

  require Logger

  defdelegate ensure_ready(_req, _rep), to: Tricorder.Grpc.Ops.EnsureReady

  defdelegate generate_signature(_req, _rep), to: Tricorder.Grpc.Ops.GenerateSignature

end
