defmodule WarpWeb.PingController do
  use WarpWeb, :controller

  def ping(conn, _params) do
    send_resp(conn, 200, "pong")
  end
end
