defmodule JleWeb.PageController do
  use JleWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
