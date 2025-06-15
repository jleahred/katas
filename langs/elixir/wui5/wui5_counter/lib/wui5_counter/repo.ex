defmodule Wui5Counter.Repo do
  use Ecto.Repo,
    otp_app: :wui5_counter,
    adapter: Ecto.Adapters.SQLite3
end
