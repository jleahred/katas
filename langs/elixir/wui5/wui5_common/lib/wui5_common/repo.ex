defmodule Wui5Common.Repo do
  use Ecto.Repo,
    otp_app: :wui5_common,
    adapter: Ecto.Adapters.SQLite3
end
