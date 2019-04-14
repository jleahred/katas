defmodule PhoenixKatasWeb.FixMsgTypesController do
  use PhoenixKatasWeb, :controller

  def fix_msg_types(conn, %{}) do
    render(conn, "fix_msg_types.html")
    # text(conn, "#{inspect(par)}")
    # text(conn, "here")
  end
end
