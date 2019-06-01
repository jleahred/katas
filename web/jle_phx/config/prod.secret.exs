use Mix.Config

# In this file, we keep production configuration that
# you'll likely want to automate and keep away from
# your version control system.
#
# You should document the content of this
# file or create a script for recreating it, since it's
# kept out of version control and might be hard to recover
# or recreate for your teammates (or yourself later on).
config :jle, JLEWeb.Endpoint,
  secret_key_base: "qjorGK1KDWkeqJg4zcrfDqIlwZZlF7Qj/Ak3uREdyF8jugHD2Y2D/UI+j6DEB5bZ"

# Configure your database
config :jle, JLE.Repo,
  username: "postgres",
  password: "postgres",
  database: "jle_prod",
  pool_size: 15
