defmodule PhoenixKatasWeb.BatchsFileControllerLV do
  use PhoenixKatasWeb, :controller
  alias Phoenix.LiveView
  require Logger

  def show(conn, par) do
    LiveView.Controller.live_render(conn, PhoenixKatasWeb.BatchsFileLive,
      session: %{
        # conn: conn,
        par: par
      }
    )

    # text(conn, "here")
  end
end
