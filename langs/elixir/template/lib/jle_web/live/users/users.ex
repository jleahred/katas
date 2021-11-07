defmodule JleWeb.UsersLive.Index do
  use JleWeb, :live_view

  alias Jle.Accounts
  alias Jle.Repo

  @impl true
  def mount(_params, _session, socket) do
    # {:ok, socket}
    {:ok,
     socket
     |> assign(:users, list_users())
     |> assign(:fields, list_fields())
     |> assign(:table_data, Repo.all(Accounts.User))
     |> assign(:remove_fields, [:__meta__, :hashed_password])}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
    # {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  # @impl true
  # def handle_event("delete", %{"id" => id}, socket) do
  #   user2 = Accounts2.get_user2!(id)
  #   {:ok, _} = Accounts2.delete_user2(user2)

  #   {:noreply, assign(socket, :users2, list_users2())}
  # end

  defp list_users do
    Repo.all(Accounts.User)
  end

  def list_fields do
    list_users()
    |> Enum.at(0)
    |> Map.from_struct()
    |> Enum.map(fn {k, _v} -> k end)
    |> Enum.filter(&(!(&1 in remove_fields())))
  end

  defp user_fields(user) do
    user
    |> Map.from_struct()
    |> Enum.filter(fn {k, _v} -> !(k in remove_fields()) end)
    |> Enum.map(fn {_k, v} -> v end)
  end

  defp remove_fields() do
    [:__meta__, :hashed_password]
  end
end
