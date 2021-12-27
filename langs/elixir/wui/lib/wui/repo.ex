defmodule Wui.Repo do
  use Ecto.Repo,
    otp_app: :wui,
    adapter: Ecto.Adapters.SQLite3
end
