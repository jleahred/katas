defmodule JLEWeb.LogsFileControllerLV do
  use JLEWeb, :controller
  alias Phoenix.LiveView
  require Logger

  def show(conn, par) do
    LiveView.Controller.live_render(conn, JLEWeb.LogsFileLive,
      session: %{
        # conn: conn,
        par: par
      }
    )

    # text(conn, "here")
  end
end
