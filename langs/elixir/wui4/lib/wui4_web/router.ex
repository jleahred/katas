defmodule Wui4Web.Router do
  use Wui4Web, :router
  import Wui4Web.RouterMacros

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {Wui4Web.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Wui4Web do
    pipe_through :browser

    get "/", PageController, :home

    # live "/counter", CounterLive, :index
  end

  scope "/" do
    pipe_through :browser

    registering_routes_start()
    live2(Wui4Web.CounterLive)
    live2(Wui4Web.Counter2Live)
    live2(Wui4Web.RoutesLive)
  end

  # Other scopes may use custom stacks.
  # scope "/api", Wui4Web do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:wui4, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: Wui4Web.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
