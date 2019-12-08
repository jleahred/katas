# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

# Configures the endpoint
config :jle2, Jle2Web.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "OKnOgzWl531tVPRVpzq5v2mWGv0ugRyMZ8HVM6ZP6KT3+TetfQgls1y3w8a6GvJd",
  render_errors: [view: Jle2Web.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Jle2.PubSub, adapter: Phoenix.PubSub.PG2],
  live_view: [
    signing_salt: "aEyGnld2rlmLrtb/xTS3ytcANIZO61I/"
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
