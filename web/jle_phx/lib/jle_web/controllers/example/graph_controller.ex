defmodule JLEWeb.ExampleGraphController do
  use JLEWeb, :controller

  def index(conn, _par) do
    render(conn, "graph.html")

    # text(conn, "#{inspect(par)}")
  end
end
