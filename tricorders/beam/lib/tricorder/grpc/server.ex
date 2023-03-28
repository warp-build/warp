defmodule Tricorder.Grpc.Server do
  use GRPC.Server, service: Build.Warp.Tricorder.TricorderService.Service

  require Logger

  defdelegate get_ast(_req, _rep), to: Tricorder.Grpc.Ops.GetAst

  defdelegate ensure_ready(_req, _rep), to: Tricorder.Grpc.Ops.EnsureReady

  defdelegate generate_signature(_req, _rep), to: Tricorder.Grpc.Ops.GenerateSignature

  defdelegate prepare_dependency(_req, _rep), to: Tricorder.Grpc.Ops.PrepareDependency
end
