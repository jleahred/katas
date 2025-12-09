defmodule Wui7Web.Router do
  use Wui7Web, :router

  import Wui7Web.UserAuth

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {Wui7Web.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :fetch_current_scope_for_user
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Wui7Web do
    pipe_through :browser

    live_session :default, on_mount: [{Wui7Web.UserAuth, :mount_current_scope}] do
      live "/", SearchLive
      live "/counter", CounterLive
      live "/counter2", Counter2Live
      live "/counter_cp", CounterCpLive
      live "/counter_cp2", CounterCp2Live
      live "/test", DiagLive
    end
  end

  scope "/", Wui7Web do
    pipe_through [:browser, :require_authenticated_user]

    live_session :admin,
      on_mount: [{Wui7Web.UserAuth, :ensure_authenticated_scope}] do
      live "/admin/users", AdminUsersLive
      live "/admin/user/:id", AdminUserLive
      live "/admin/roles", AdminRolesLive
    end
  end

  # Other scopes may use custom stacks.
  # scope "/api", Wui7Web do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:wui7, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: Wui7Web.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end

  ## Authentication routes

  scope "/", Wui7Web do
    pipe_through [:browser, :redirect_if_user_is_authenticated]

    get "/users/register", UserRegistrationController, :new
    post "/users/register", UserRegistrationController, :create
  end

  scope "/", Wui7Web do
    pipe_through [:browser, :require_authenticated_user]

    get "/users/settings", UserSettingsController, :edit
    put "/users/settings", UserSettingsController, :update
    get "/users/settings/confirm-email/:token", UserSettingsController, :confirm_email
  end

  scope "/", Wui7Web do
    pipe_through [:browser]

    get "/users/log-in", UserSessionController, :new
    get "/users/log-in/:token", UserSessionController, :confirm
    post "/users/log-in", UserSessionController, :create
    delete "/users/log-out", UserSessionController, :delete
  end
end
