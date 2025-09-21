defmodule Wui6Web.AdminUserLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts

  # @metadata %{
  #   title: "Admin · User",
  #   description: "Detalle y gestión de roles para un usuario concreto.",
  #   keywords: ["admin", "user", "roles"],
  # }

  # def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Usuario")
     |> assign(:user, nil)
     |> assign(:user_roles, [])
     |> assign(:available_roles, [])
     |> assign(:return_to, nil)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    id_param = Map.get(params, "id", "1")
    current_return = socket.assigns[:return_to]
    return_to_param = Map.get(params, "return_to")

    return_to =
      return_to_param
      |> decode_return_to(current_return)
      |> safe_return_to()

    case Accounts.get_user_with_roles(id_param) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:return_to, return_to)
          |> assign_user(user)

        cond do
          to_string(user.id) != to_string(id_param) ->
            {:noreply, push_patch(socket, to: ~p"/admin/user/#{user.id}", replace: true)}

          return_to_param ->
            {:noreply, push_patch(socket, to: ~p"/admin/user/#{user.id}", replace: true)}

          true ->
            {:noreply, socket}
        end

      {:error, :not_found} ->
        {:noreply,
         socket
         |> assign(:return_to, return_to)
         |> put_flash(:error, "No hay usuarios disponibles")
         |> assign(:user, nil)
         |> assign(:user_roles, [])
         |> assign(:available_roles, [])}
    end
  end

  @impl true
  def handle_event(
        "add_role",
        %{"role-id" => role_id},
        %{assigns: %{user: %{id: user_id}}} = socket
      ) do
    with {:ok, role_id} <- parse_positive_int(role_id),
         :ok <- Accounts.assign_role_to_user(user_id, role_id),
         {:ok, user} <- Accounts.get_user_with_roles(user_id) do
      {:noreply, assign_user(socket, user)}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, put_flash(socket, :error, format_changeset_error(changeset))}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_event("add_role", _params, socket), do: {:noreply, socket}

  @impl true
  def handle_event(
        "remove_role",
        %{"role-id" => role_id},
        %{assigns: %{user: %{id: user_id}}} = socket
      ) do
    with {:ok, role_id} <- parse_positive_int(role_id),
         :ok <- Accounts.remove_role_from_user(user_id, role_id),
         {:ok, user} <- Accounts.get_user_with_roles(user_id) do
      {:noreply, assign_user(socket, user)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("remove_role", _params, socket), do: {:noreply, socket}

  @impl true
  def handle_event("toggle_enabled", _params, %{assigns: %{user: %{id: user_id}}} = socket) do
    case Accounts.toggle_user_enabled(user_id) do
      {:ok, _updated_user} ->
        with {:ok, user} <- Accounts.get_user_with_roles(user_id) do
          {:noreply, assign_user(socket, user)}
        else
          _ ->
            {:noreply, socket}
        end

      {:error, _reason} ->
        {:noreply, put_flash(socket, :error, "No se pudo actualizar el estado del usuario")}
    end
  end

  def handle_event("toggle_enabled", _params, socket), do: {:noreply, socket}

  defp assign_user(socket, user) do
    roles = user.roles || []

    socket
    |> assign(:user, user)
    |> assign(:user_roles, Enum.sort_by(roles, &String.downcase(&1.name || "")))
    |> assign(:available_roles, Accounts.available_roles_for_user(%{user | roles: roles}))
  end

  defp decode_return_to(nil, fallback), do: fallback

  defp decode_return_to(value, _fallback) do
    URI.decode_www_form(value)
  rescue
    _ -> value
  end

  defp safe_return_to(path) when is_binary(path) do
    if String.starts_with?(path, "/"), do: path, else: default_return_to()
  end

  defp safe_return_to(path) when path in [nil, ""], do: nil
  defp safe_return_to(_), do: default_return_to()

  defp default_return_to, do: ~p"/admin/users"

  defp parse_positive_int(value) when is_integer(value) and value > 0, do: {:ok, value}

  defp parse_positive_int(value) do
    value
    |> to_string()
    |> Integer.parse()
    |> case do
      {int, ""} when int > 0 -> {:ok, int}
      _ -> {:error, :invalid_integer}
    end
  end

  defp format_changeset_error(%Ecto.Changeset{errors: errors}) do
    errors
    |> Enum.map(fn {field, {message, _}} ->
      "#{Phoenix.Naming.humanize(field)} #{message}"
    end)
    |> Enum.join(", ")
  end

  defp format_datetime(nil), do: "—"

  defp format_datetime(%NaiveDateTime{} = datetime) do
    datetime
    |> NaiveDateTime.truncate(:second)
    |> Calendar.strftime("%Y-%m-%d %H:%M:%S")
  end

  defp format_datetime(%DateTime{} = datetime) do
    datetime
    |> DateTime.truncate(:second)
    |> Calendar.strftime("%Y-%m-%d %H:%M:%S")
  end

  defp format_datetime(other) when is_binary(other), do: other
  defp format_datetime(other), do: to_string(other)

  defp enabled_badge(true), do: {"Activo", "badge badge-success"}
  defp enabled_badge(false), do: {"Inactivo", "badge badge-ghost"}
end
