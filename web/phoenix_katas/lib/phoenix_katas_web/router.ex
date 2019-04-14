defmodule PhoenixKatasWeb.Router do
  use PhoenixKatasWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", PhoenixKatasWeb do
    pipe_through :browser

    get "/", MainPageController, :index
    # get "/search", SearchController, :search, as: :search
    get "/factorial", FactorialController, :factorial
    get "/fix/tags", FixTagsController, :fix_tags
    get "/fix/msg_types", FixMsgTypesController, :fix_msg_types
    get "/fix/log", FixLogController, :fix_log
  end

  # Other scopes may use custom stacks.
  # scope "/api", PhoenixKatasWeb do
  #   pipe_through :api
  # end
end
