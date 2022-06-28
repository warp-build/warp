defmodule Zap.SharedCache.Get do
  require Logger

  def get(hash) do
    Logger.info("getting asset by hash: #{hash}")
    if Zap.SharedCache.Artifactory.exists?(hash) do
      Logger.info("asset with hash #{hash} exists!")
      Zap.SharedCache.Artifactory.presigned_read_url(hash)
    else
      {:error, :not_found}
    end
  end

end
