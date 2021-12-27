defmodule WuiWeb.XxxNewOperLive do
  use WuiWeb, :live_view

  def mount(_session, _, socket) do
    {
      :ok,
      socket
      |> assign(:debug, "debug")
    }
  end
end
