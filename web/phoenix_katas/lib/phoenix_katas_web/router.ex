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
  end

  # Other scopes may use custom stacks.
  # scope "/api", PhoenixKatasWeb do
  #   pipe_through :api
  # end
end
