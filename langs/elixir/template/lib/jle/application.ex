defmodule Jle.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      Jle.Repo,
      # Start the Telemetry supervisor
      JleWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Jle.PubSub},
      # Start the Endpoint (http/https)
      JleWeb.Endpoint
      # Start a worker by calling: Jle.Worker.start_link(arg)
      # {Jle.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Jle.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    JleWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
