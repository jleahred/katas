defmodule Wui5MainWeb.PageController do
  use Wui5MainWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
