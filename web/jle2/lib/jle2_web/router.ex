defmodule Jle2Web.Router do
  use Jle2Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug Phoenix.LiveView.Flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug Jle2Web.PlugRedirect
    plug Jle2Web.PlugLog
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Jle2Web do
    pipe_through :browser

    get "/", PageController, :index

    live "/counter", CounterLive
    live "/redirect", CounterLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", Jle2Web do
  #   pipe_through :api
  # end
end
