defmodule JLEWeb.MainPageController do
  use JLEWeb, :controller

  def index(conn, %{"txt" => txt}) do
    render(conn, "index.html", txt: txt)
  end

  def index(conn, %{}) do
    render(conn, "index.html", txt: "")
  end

  # def index(conn, _params) do
  #   # redirect(conn, to: "/search")
  #   render(conn, "index.html")
  # end
end
