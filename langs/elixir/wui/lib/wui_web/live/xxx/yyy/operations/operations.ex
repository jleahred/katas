defmodule WuiWeb.OperationsLive do
  use WuiWeb, :live_view

  # def render(assigns) do
  #   ~H"""
  #   Hello there
  #   """
  # end

  def mount(_params, session, socket) do
    # if connected?(socket), do: Process.send_after(self(), :tick, 1000)

    {
      :ok,
      socket
      |> assign(debug: session)
      #
    }
  end
end
