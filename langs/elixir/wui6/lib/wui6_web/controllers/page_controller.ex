defmodule Wui6Web.PageController do
  use Wui6Web, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
