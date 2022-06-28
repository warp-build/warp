defmodule Zap.Repo do
  use Ecto.Repo,
    otp_app: :zap,
    adapter: Ecto.Adapters.Postgres
end
