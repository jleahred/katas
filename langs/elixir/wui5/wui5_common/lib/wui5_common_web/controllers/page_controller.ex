defmodule Wui5CommonWeb.PageController do
  use Wui5CommonWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
