defmodule Wui6Web.Router do
  use Wui6Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {Wui6Web.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug Wui6Web.Plugs.RegisterSession
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Wui6Web do
    pipe_through :browser

    live "/", HomeLive
    live "/counter1", CounterLive
    live "/counter2", Counter2Live
    live "/technology", TechnologyLive
    live "/users/activity", UsersActivityLive
    live "/user", UserLive
    get "/logout", SessionController, :logout
    live "/admin/user/register", AdminUserRegisterLive
    live "/admin/sessions", AdminSessionsLive
    live "/admin/users", AdminUsersLive
    live "/admin/user/:id", AdminUserLive
    live "/admin/roles", AdminRolesLive

    get "/admin/register_session/:token", RegisterSessionController, :show
  end

  # Other scopes may use custom stacks.
  # scope "/api", Wui6Web do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:wui6, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: Wui6Web.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
