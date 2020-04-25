defmodule JLEWeb.Router do
  use JLEWeb, :router

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

  scope "/", JLEWeb do
    pipe_through(:browser)

    live "/", MainPageLive
    live "/dashboard", DashBoardLive

    # get "/", MainPageController, :index
    # get "/search", SearchController, :search, as: :search
    get "/live/fix/log", FixLogControllerLV, :show
    get "/fix/log", FixLogController, :fix_log
    get "/fix/log/msg/:idmsg", FixLogMsgController, :show
    get "/fix/log/clordid/", FixLogClOrdIdController, :show
    get "/fix/static_tables/:type", FixStaticTableController, :index
    get "/fix/beautifier", FixBeautifierController, :index

    get "/cws/versions", CwsVersionsController, :index
    get "/cws/stats/delay", CwsStatsDelayController, :index
    get "/price_grants", PriceGrantsController, :index

    live "/live/counter", CounterLive
    live "/live/factorial", FactorialLive
    get "/factorial", FactorialController, :factorial
    get "/example/graph", ExampleGraphController, :index

    live "/logs", LogsLive
    # live "/logs/file/:file_name", LogsFileLive
    get "/logs/file", LogsFileControllerLV, :show
    live "/pubsub/ex", PubSubExLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", JLEWeb do
  #   pipe_through :api
  # end
end
