defmodule ZapWeb.Router do
  use ZapWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", ZapWeb do
    pipe_through :api

    get "/ping", PingController, :ping

    post "/artifact/:hash", ArtifactController, :upload

    get "/artifact/:hash", ArtifactController, :get
  end
end
