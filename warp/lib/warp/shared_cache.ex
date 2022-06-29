defmodule Warp.SharedCache do

  defdelegate upload_url(uri), to: Warp.SharedCache.Upload

  defdelegate get(hash), to: Warp.SharedCache.Get

end
