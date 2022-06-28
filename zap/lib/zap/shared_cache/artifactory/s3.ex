defmodule Zap.SharedCache.Artifactory.S3 do
  require Logger

  def new, do: ExAws.Config.new(:s3)

  def bucket_name, do: Application.get_env(:zap, SharedCache.Artifactory.S3)[:bucket_name]

  def exists?(store, path) do
    case ExAws.S3.head_object(bucket_name(), path) |> ExAws.request() do
      {:ok, %{status_code: 200}} -> true
      _ -> false
    end
  end

  def presigned_read_url(store, path) do
    store |> ExAws.S3.presigned_url(:get, bucket_name(), path, [
        s3_accelerate: false,
        expires_in: 60 * 1
      ])
  end

  def presigned_write_url(store, path) do
    store |> ExAws.S3.presigned_url(:put, bucket_name(), path, [
        s3_accelerate: false,
        expires_in: 60 * 1
      ])
  end

end
