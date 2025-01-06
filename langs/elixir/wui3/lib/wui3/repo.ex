defmodule Wui3.Repo do
  use Ecto.Repo,
    otp_app: :wui3,
    adapter: Ecto.Adapters.SQLite3
end
