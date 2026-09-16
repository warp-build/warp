defmodule Tricorder.Grpc.Ops.EnsureReady do
  def ensure_ready(_req, _stream) do
    loop()
  end

  def loop do
    if Tricorder.Deps.loaded?() do
      Build.Warp.Tricorder.EnsureReadyResponse.new()
    else
      loop()
    end
  end
end
