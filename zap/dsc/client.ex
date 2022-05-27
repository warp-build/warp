defmodule Zap.Dsc.Client do

  defdelegate get(req, ctx), to: Zap.Dsc.SharedCache
  defdelegate put(req, ctx), to: Zap.Dsc.SharedCache
  defdelegate clear(req, ctx), to: Zap.Dsc.SharedCache

end
