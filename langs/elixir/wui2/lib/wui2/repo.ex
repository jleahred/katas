defmodule Wui2.Repo do
  use Ecto.Repo,
    otp_app: :wui2,
    adapter: Ecto.Adapters.SQLite3
end
