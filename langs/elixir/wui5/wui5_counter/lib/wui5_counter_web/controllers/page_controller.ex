defmodule Wui5CounterWeb.PageController do
  use Wui5CounterWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
