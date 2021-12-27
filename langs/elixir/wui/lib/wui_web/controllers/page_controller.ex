defmodule WuiWeb.PageController do
  use WuiWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
