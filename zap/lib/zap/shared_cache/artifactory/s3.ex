defmodule Zap.SharedCache.Artifactory.S3 do
  require Logger

  def new, do: ExAws.Config.new(:s3)

  def bucket_name, do: Application.get_env(:zap, SharedCache.Artifactory.S3)[:bucket_name]

  def exists?(_store, path) do
    case ExAws.S3.head_object(bucket_name(), path) |> ExAws.request() do
      {:ok, %{status_code: 200}} -> true
      _ -> false
    end
  end

  def presigned_read_url(store, path) do
    opts = [ expires_in: 60 * 10 ]
    ExAws.S3.presigned_url(store, :get, bucket_name(), path, opts)
  end

  def presigned_write_url(store, path) do
    headers = [
      {"Content-Type", "application/octet-stream"},
      {"ACL", "public-read"}
    ]

    opts = [ headers: headers ]

    ExAws.S3.presigned_url(store, :put, bucket_name(), path, opts)
  end

end
