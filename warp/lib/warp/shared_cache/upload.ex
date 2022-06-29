defmodule Warp.SharedCache.Upload do

  def upload_url(hash) do
    # register events
    # save stuff to the database
    Warp.SharedCache.Artifactory.presigned_write_url(hash)
  end

end
