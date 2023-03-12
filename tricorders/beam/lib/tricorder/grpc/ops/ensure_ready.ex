defmodule Tricorder.Grpc.Ops.EnsureReady do
  def ensure_ready(_req, _stream) do
    Build.Warp.Tricorder.EnsureReadyResponse.new()
  end
end
