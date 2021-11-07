defmodule JleWeb.User2Live.FormComponent do
  use JleWeb, :live_component

  alias Jle.Accounts2

  @impl true
  def update(%{user2: user2} = assigns, socket) do
    changeset = Accounts2.change_user2(user2)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(:changeset, changeset)}
  end

  @impl true
  def handle_event("validate", %{"user2" => user2_params}, socket) do
    changeset =
      socket.assigns.user2
      |> Accounts2.change_user2(user2_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save", %{"user2" => user2_params}, socket) do
    save_user2(socket, socket.assigns.action, user2_params)
  end

  defp save_user2(socket, :edit, user2_params) do
    case Accounts2.update_user2(socket.assigns.user2, user2_params) do
      {:ok, _user2} ->
        {:noreply,
         socket
         |> put_flash(:info, "User2 updated successfully")
         |> push_redirect(to: socket.assigns.return_to)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp save_user2(socket, :new, user2_params) do
    case Accounts2.create_user2(user2_params) do
      {:ok, _user2} ->
        {:noreply,
         socket
         |> put_flash(:info, "User2 created successfully")
         |> push_redirect(to: socket.assigns.return_to)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end
