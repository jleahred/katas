defmodule Wui4Web.PageController do
  use Wui4Web, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
