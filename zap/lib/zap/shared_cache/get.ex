defmodule Zap.SharedCache.Get do

  def get(hash) do
    Zap.SharedCache.Artifactory.presigned_read_url(hash)
  end

end
