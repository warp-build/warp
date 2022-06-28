defmodule ZapWeb.ArtifactController do
  use ZapWeb, :controller

  def upload(conn, %{ "hash" => hash }) do
    {:ok, signed_url} = Zap.SharedCache.upload_url(hash)
    json(conn, %{ signed_url: signed_url})
  end

  def get(conn, %{ "hash" => hash }) do
    {:ok, asset_url} = Zap.SharedCache.get(hash)
    redirect(conn, external: asset_url)
  end
end
