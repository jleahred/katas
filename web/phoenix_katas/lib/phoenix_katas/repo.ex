defmodule PhoenixKatas.Repo do
  use Ecto.Repo,
    otp_app: :phoenix_katas,
    adapter: Ecto.Adapters.MySQL
end
