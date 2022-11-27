defmodule Wui2.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      Wui2Web.Telemetry,
      # Start the Ecto repository
      Wui2.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: Wui2.PubSub},
      # Start Finch
      {Finch, name: Wui2.Finch},
      # Start the Endpoint (http/https)
      Wui2Web.Endpoint
      # Start a worker by calling: Wui2.Worker.start_link(arg)
      # {Wui2.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Wui2.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    Wui2Web.Endpoint.config_change(changed, removed)
    :ok
  end
end
