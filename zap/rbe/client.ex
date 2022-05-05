defmodule Zap.Rbe.Client do

  defdelegate queue_build(req, ctx), to: Zap.Rbe.BuildQueue

end
