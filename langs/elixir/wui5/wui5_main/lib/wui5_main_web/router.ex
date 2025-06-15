defmodule Wui5MainWeb.Router do
  use Wui5MainWeb, :router
  import Wui5CommonWeb.RouterMacros

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {Wui5MainWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # scope "/", Wui5MainWeb do
  #   pipe_through :browser

  #   get "/", PageController, :home
  # end

  registering_routes_start()

  scope2 "/" do
    pipe_through :browser

    live2(Wui5CommonWeb.RoutesLive)
  end

  # Other scopes may use custom stacks.
  # scope "/api", Wui5MainWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:wui5_main, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: Wui5MainWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
