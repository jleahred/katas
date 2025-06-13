defmodule Wui4.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Wui4Web.Telemetry,
      Wui4.Repo,
      {Ecto.Migrator,
       repos: Application.fetch_env!(:wui4, :ecto_repos), skip: skip_migrations?()},
      {DNSCluster, query: Application.get_env(:wui4, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Wui4.PubSub},
      # Start a worker by calling: Wui4.Worker.start_link(arg)
      # {Wui4.Worker, arg},
      # Start to serve requests, typically the last entry
      Wui4Web.Endpoint,
      Wui4Web.RouteRegistry
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Wui4.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    Wui4Web.Endpoint.config_change(changed, removed)
    :ok
  end

  defp skip_migrations?() do
    # By default, sqlite migrations are run when using a release
    System.get_env("RELEASE_NAME") == nil
  end
end
