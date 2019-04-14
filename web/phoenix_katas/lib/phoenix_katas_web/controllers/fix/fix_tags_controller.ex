defmodule PhoenixKatasWeb.FixTagsController do
  use PhoenixKatasWeb, :controller

  def fix_tags(conn, %{}) do
    render(conn, "fix_tags.html")
    # text(conn, "#{inspect(par)}")
  end
end
