defmodule Wui5Main.Repo do
  use Ecto.Repo,
    otp_app: :wui5_main,
    adapter: Ecto.Adapters.SQLite3
end
