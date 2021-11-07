defmodule JleWeb.User2Live.Show do
  use JleWeb, :live_view

  alias Jle.Accounts2

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_params(%{"id" => id}, _, socket) do
    {:noreply,
     socket
     |> assign(:page_title, page_title(socket.assigns.live_action))
     |> assign(:user2, Accounts2.get_user2!(id))}
  end

  defp page_title(:show), do: "Show User2"
  defp page_title(:edit), do: "Edit User2"
end
