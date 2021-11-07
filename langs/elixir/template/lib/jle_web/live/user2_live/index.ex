defmodule JleWeb.User2Live.Index do
  use JleWeb, :live_view

  alias Jle.Accounts2
  alias Jle.Accounts2.User2

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :users2, list_users2())}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Edit User2")
    |> assign(:user2, Accounts2.get_user2!(id))
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New User2")
    |> assign(:user2, %User2{})
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Users2")
    |> assign(:user2, nil)
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    user2 = Accounts2.get_user2!(id)
    {:ok, _} = Accounts2.delete_user2(user2)

    {:noreply, assign(socket, :users2, list_users2())}
  end

  defp list_users2 do
    Accounts2.list_users2()
  end
end
