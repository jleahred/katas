defmodule PhoenixKatasWeb.Router do
  use PhoenixKatasWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug Phoenix.LiveView.Flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/", PhoenixKatasWeb do
    pipe_through(:browser)

    live "/", MainPageLive

    # get "/", MainPageController, :index
    # get "/search", SearchController, :search, as: :search
    get "/factorial", FactorialController, :factorial
    get "/fix/log", FixLogController, :fix_log
    get "/fix/log/msg/:idmsg", FixLogMsgController, :show
    get("/fix/log/clordid/", FixLogClOrdIdController, :show)
    get("/fix/static_tables/:type", FixStaticTableController, :index)
    get("/cws/versions", CwsVersionsController, :index)

    live "/live/counter", CounterLive
    live "/live/factorial", FactorialLive

    get "/live/fix/log", FixLogControllerLV, :show

    get "/example/graph", ExampleGraphController, :index

    # live "/live/fix/log", FixLogLive
    # live("/", MainPageLive)
  end

  # Other scopes may use custom stacks.
  # scope "/api", PhoenixKatasWeb do
  #   pipe_through :api
  # end
end
