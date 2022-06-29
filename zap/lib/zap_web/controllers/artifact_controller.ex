defmodule ZapWeb.ArtifactController do
  require Logger

  use ZapWeb, :controller

  def upload(conn, %{ "hash" => hash }) do
    # artefact = Zap.Artefact.from_map(%{ hash: hash, host_triple: host_triple })
    {:ok, signed_url} = Zap.SharedCache.upload_url("#{hash}")
    Logger.info("Signed URL: #{signed_url}")
    json(conn, %{ signed_url: signed_url })
  end

  def get(conn, %{ "hash" => hash }) do
    case Zap.SharedCache.get("#{hash}") do
      {:ok, asset_url} -> redirect(conn, external: asset_url)
      {:error, :not_found} -> Plug.Conn.send_resp(conn, 404, "")
    end
  end
end
