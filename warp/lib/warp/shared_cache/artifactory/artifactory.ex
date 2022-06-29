defmodule Warp.SharedCache.Artifactory do

  defp store, do: Application.get_env(:warp, SharedCache.Artifactory)[:store]

  def exists?(hash) do
    s = store().new()
    store().exists?(s, hash)
  end

  def presigned_read_url(hash) do
    s = store().new()
    store().presigned_read_url(s, hash)
  end

  def presigned_write_url(hash) do
    s = store().new()
    store().presigned_write_url(s, hash)
  end

end

