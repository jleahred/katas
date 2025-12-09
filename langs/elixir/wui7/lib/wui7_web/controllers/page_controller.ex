defmodule Wui7Web.PageController do
  use Wui7Web, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
