defmodule Wui2Web.AdminHome do
  use Wui2Web, :live_view
  # alias Wui2Web.Router.Helpers, as: Routes

  # def render(assigns) do
  #   ~H"""
  #   Hello there
  #   """
  # end

  def mount(_params, session, socket) do
    # if connected?(socket), do: Process.send_after(self(), :tick, 1000)

    # an = socket.private.assign_new |> elem(0)
    # current_user = an.current_user.email

    {
      :ok,
      socket
      |> assign(debug: session)
    }
  end
end
