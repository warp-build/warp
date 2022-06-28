defmodule Zap.SharedCache.Upload do

  def upload_url(hash) do
    aws = ExAws.Config.new(:s3)

    {:ok, signed_url} = ExAws.S3.presigned_url(
      aws,
      :put,
      "local.zap.artefacts",
      hash,
      [
        query_params: [{"ACL", "public-read"}],
        s3_accelerate: false,
        expires_in: 60 * 5 # 5 minutes
      ]
    )

    {:ok, %{ signed_url: signed_url }}
  end

end
