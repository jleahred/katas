defmodule Wui6.Repo do
  use Ecto.Repo,
    otp_app: :wui6,
    adapter: Ecto.Adapters.SQLite3
end
