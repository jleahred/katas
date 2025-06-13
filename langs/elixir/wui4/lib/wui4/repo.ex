defmodule Wui4.Repo do
  use Ecto.Repo,
    otp_app: :wui4,
    adapter: Ecto.Adapters.SQLite3
end
