defmodule Zap.SharedCache.Artifactory do

  defp store(), do: Application.get_env(:zap, SharedCache.Artifactory)[:store]

  def presigned_read_url(hash) do
    s = store().new()
    store().presigned_read_url(s, hash)
  end

  def presigned_write_url(hash) do
    s = store().new()
    store().presigned_read_url(s, hash)
  end

end

