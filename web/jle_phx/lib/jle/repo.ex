defmodule JLE.RepoTrading do
  use Ecto.Repo,
    otp_app: :jle,
    adapter: Ecto.Adapters.MySQL
end

defmodule JLE.RepoAlarms do
  use Ecto.Repo,
    otp_app: :jle,
    adapter: Ecto.Adapters.MySQL
end
