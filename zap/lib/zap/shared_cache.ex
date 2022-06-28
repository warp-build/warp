defmodule Zap.SharedCache do

  defdelegate upload_url(uri), to: Zap.SharedCache.Upload

  defdelegate get(hash), to: Zap.SharedCache.Get

end
