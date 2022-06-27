defmodule Zap.SharedCache do

  defdelegate upload_url(uri), to: Zap.SharedCache.Upload

end
