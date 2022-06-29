defmodule Warp.Repo do
  use Ecto.Repo,
    otp_app: :warp,
    adapter: Ecto.Adapters.Postgres
end
