defmodule PhoenixKatas.RepoTrading do
  use Ecto.Repo,
    otp_app: :phoenix_katas,
    adapter: Ecto.Adapters.MySQL
end

defmodule PhoenixKatas.RepoAlarms do
  use Ecto.Repo,
    otp_app: :phoenix_katas,
    adapter: Ecto.Adapters.MySQL
end
