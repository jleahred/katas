defmodule Wui7.Repo do
  use Ecto.Repo,
    otp_app: :wui7,
    adapter: Ecto.Adapters.SQLite3
end
