# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :phoenix_katas,
  ecto_repos: [PhoenixKatas.Repo]

# Configures the endpoint
config :phoenix_katas, PhoenixKatasWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "a+Wla5Ir0t2jHwdXx15oeUElHQ0RwEbHaW48I0FZzmDlWrfTpKhneVF1wS+65yxZ",
  render_errors: [view: PhoenixKatasWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PhoenixKatas.PubSub, adapter: Phoenix.PubSub.PG2],
  live_view: [
    signing_salt: "B18AW1EAWRrvwLG8V4CA9ggF/ginisoq"
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
