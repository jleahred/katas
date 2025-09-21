defmodule Wui6Web.AdminRolesLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts
  alias Wui6.Roles
  alias Wui6.Roles.Role

  @metadata %{
    title: "Admin · Edit roles",
    keywords: "admin roles",
    description: """
    LiveView for managing user roles in the admin panel.
    """
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Roles")
     |> assign(:roles, load_roles())
     |> assign(:system_roles, Roles.system_roles())
     |> assign(:form_role, %Role{})
     |> assign(:form_changeset, Roles.change_role(%Role{}))
     |> assign(:form_mode, :new)
     |> assign(:show_form_modal, false)
     |> assign(:selected_role_id, nil)
     |> assign(:selected_role, nil)
     |> assign(:selected_role_users, [])
     |> assign(:user_search_query, "")
     |> assign(:user_search_results, [])
     |> assign(:show_users_modal, false)
     |> assign_current_path(%{})}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    modal = Map.get(params, "modal")

    case modal do
      "new" ->
        {:noreply,
         socket
         |> assign(:form_role, %Role{})
         |> assign(:form_mode, :new)
         |> assign(:form_changeset, Roles.change_role(%Role{}))
         |> assign(:show_form_modal, true)
         |> assign(:show_users_modal, false)
         |> assign(:selected_role_id, nil)
         |> assign(:selected_role, nil)
         |> assign(:selected_role_users, [])
         |> assign(:user_search_query, "")
         |> assign(:user_search_results, [])
         |> assign_current_path(params)}

      "edit" ->
        with {:ok, role_id} <- parse_positive_int(Map.get(params, "id")),
             %Role{} = role <- Roles.get_role(role_id) do
          {:noreply,
           socket
           |> assign(:form_role, role)
           |> assign(:form_mode, :edit)
           |> assign(:form_changeset, Roles.change_role(role))
           |> assign(:show_form_modal, true)
           |> assign(:show_users_modal, false)
           |> assign(:selected_role_id, nil)
           |> assign(:selected_role, nil)
           |> assign(:selected_role_users, [])
           |> assign(:user_search_query, "")
           |> assign(:user_search_results, [])
           |> assign_current_path(params)}
        else
          _ ->
            {:noreply, close_modals(socket)}
        end

      "users" ->
        with {:ok, role_id} <- parse_positive_int(Map.get(params, "id")),
             %Role{} = role <- find_role(socket.assigns.roles, role_id) do
          email_filter = params |> Map.get("email", "") |> to_string() |> String.trim()

          search_results =
            if email_filter == "" do
              []
            else
              Accounts.search_users_for_role(role_id, limit: 3, query: email_filter)
            end

          sanitized_params = Map.put(params, "email", email_filter)

          {:noreply,
           socket
           |> assign(:selected_role_id, role_id)
           |> assign(:selected_role, role)
           |> assign(:selected_role_users, Accounts.list_users_for_role(role_id))
           |> assign(:user_search_query, email_filter)
           |> assign(:user_search_results, search_results)
           |> assign(:show_users_modal, true)
           |> assign(:show_form_modal, false)
           |> assign_current_path(sanitized_params)}
        else
          _ ->
            {:noreply, close_modals(socket)}
        end

      _ ->
        {:noreply, close_modals(socket)}
    end
  end

  @impl true
  def handle_event("new", _params, socket) do
    {:noreply, push_patch(socket, to: ~p"/admin/roles?modal=new")}
  end

  def handle_event("edit", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: ~p"/admin/roles?#{[modal: "edit", id: id]}")}
  end

  def handle_event("cancel", _params, socket) do
    {:noreply, push_patch(socket, to: ~p"/admin/roles")}
  end

  def handle_event("validate", %{"role" => params}, socket) do
    changeset =
      socket.assigns.form_role
      |> Roles.change_role(params)
      |> Map.put(:action, :validate)

    {:noreply,
     socket
     |> assign(:form_changeset, changeset)
     |> assign(:show_form_modal, true)}
  end

  def handle_event("save", %{"role" => params}, %{assigns: %{form_mode: :new}} = socket) do
    case Roles.create_role(params) do
      {:ok, _role} ->
        {:noreply,
         socket
         |> put_flash(:info, "Rol creado")
         |> assign(:roles, load_roles())
         |> refresh_selected_role()
         |> push_patch(to: ~p"/admin/roles")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply,
         socket
         |> assign(:form_changeset, Map.put(changeset, :action, :insert))
         |> assign(:show_form_modal, true)}
    end
  end

  def handle_event(
        "save",
        %{"role" => params},
        %{assigns: %{form_mode: :edit, form_role: role}} = socket
      ) do
    case Roles.update_role(role, params) do
      {:ok, updated_role} ->
        {:noreply,
         socket
         |> put_flash(:info, "Rol actualizado")
         |> assign(:roles, replace_role(socket.assigns.roles, updated_role))
         |> refresh_selected_role()
         |> push_patch(to: ~p"/admin/roles")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply,
         socket
         |> assign(:form_changeset, Map.put(changeset, :action, :update))
         |> assign(:show_form_modal, true)}
    end
  end

  def handle_event("show-role-users", %{"id" => id_param}, socket) do
    {:noreply, push_patch(socket, to: ~p"/admin/roles?#{[modal: "users", id: id_param]}")}
  end

  def handle_event("close-modals", _params, socket) do
    {:noreply, push_patch(socket, to: ~p"/admin/roles")}
  end

  def handle_event("noop", _params, socket), do: {:noreply, socket}

  def handle_event("search_users", params, %{assigns: %{selected_role_id: nil}} = socket) do
    trimmed = params |> extract_search_query() |> String.trim()

    {:noreply,
     socket
     |> assign(:user_search_query, trimmed)
     |> assign(:user_search_results, [])}
  end

  def handle_event("search_users", params, %{assigns: %{selected_role_id: role_id}} = socket)
      when not is_nil(role_id) do
    trimmed = params |> extract_search_query() |> String.trim()

    results =
      if trimmed == "" do
        []
      else
        Accounts.search_users_for_role(role_id, limit: 3, query: trimmed)
      end

    param_map = users_modal_params(role_id, trimmed)

    {:noreply,
     socket
     |> assign(:user_search_query, trimmed)
     |> assign(:user_search_results, results)
     |> assign_current_path(param_map)
     |> push_patch(to: roles_path(param_map))}
  end

  def handle_event("search_users", _params, socket), do: {:noreply, socket}

  def handle_event(
        "add_user_to_role",
        %{"user-id" => user_id_param},
        %{assigns: %{selected_role_id: role_id}} = socket
      )
      when not is_nil(role_id) do
    with {:ok, user_id} <- parse_positive_int(user_id_param),
         :ok <- Accounts.assign_role_to_user(user_id, role_id) do
      query = socket.assigns.user_search_query

      socket =
        socket
        |> assign(:roles, load_roles())
        |> refresh_selected_role()

      results =
        if query == "" do
          []
        else
          Accounts.search_users_for_role(role_id, limit: 3, query: query)
        end

      param_map = users_modal_params(role_id, query)

      {:noreply,
       socket
       |> assign(:user_search_results, results)
       |> assign_current_path(param_map)
       |> push_patch(to: roles_path(param_map))}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("add_user_to_role", _params, socket), do: {:noreply, socket}

  def handle_event(
        "remove_user_from_role",
        %{"user-id" => user_id_param},
        %{assigns: %{selected_role_id: role_id}} = socket
      )
      when not is_nil(role_id) do
    with {:ok, user_id} <- parse_positive_int(user_id_param),
         :ok <- Accounts.remove_role_from_user(user_id, role_id) do
      query = socket.assigns.user_search_query

      socket =
        socket
        |> assign(:roles, load_roles())
        |> refresh_selected_role()

      results =
        if query == "" do
          []
        else
          Accounts.search_users_for_role(role_id, limit: 3, query: query)
        end

      param_map = users_modal_params(role_id, query)

      {:noreply,
       socket
       |> assign(:user_search_results, results)
       |> assign_current_path(param_map)
       |> push_patch(to: roles_path(param_map))}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("remove_user_from_role", _params, socket), do: {:noreply, socket}

  def handle_event("delete", %{"id" => id}, socket) do
    role = Roles.get_role!(id)

    case Roles.delete_role(role) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:info, "Rol eliminado")
         |> assign(:roles, Enum.reject(socket.assigns.roles, &(&1.id == role.id)))
         |> maybe_reset_form(role.id)
         |> unselect_role_if_removed(role.id)
         |> assign(:user_search_query, "")
         |> assign(:user_search_results, [])
         |> push_patch(to: ~p"/admin/roles")}

      {:error, :role_in_use} ->
        {:noreply,
         socket
         |> put_flash(:error, "No se puede eliminar un rol asignado a usuarios")
         |> assign(:show_form_modal, false)}
    end
  end

  defp load_roles do
    Roles.list_roles()
  end

  defp replace_role(roles, updated_role) do
    Enum.map(roles, fn role -> if role.id == updated_role.id, do: updated_role, else: role end)
  end

  defp maybe_reset_form(socket, removed_id) do
    case socket.assigns do
      %{form_mode: :edit, form_role: %{id: ^removed_id}} ->
        close_modals(socket)

      _ ->
        socket
    end
  end

  defp reset_form(socket) do
    socket
    |> assign(:form_role, %Role{})
    |> assign(:form_mode, :new)
    |> assign(:form_changeset, Roles.change_role(%Role{}))
    |> assign(:show_form_modal, false)
  end

  defp refresh_selected_role(%{assigns: %{selected_role_id: nil}} = socket) do
    socket
  end

  defp refresh_selected_role(%{assigns: %{selected_role_id: selected_id, roles: roles}} = socket) do
    case find_role(roles, selected_id) do
      %Role{} = role ->
        socket
        |> assign(:selected_role, role)
        |> assign(:selected_role_users, Accounts.list_users_for_role(role.id))

      _ ->
        socket
    end
  end

  defp refresh_selected_role(socket), do: socket

  defp unselect_role_if_removed(socket, removed_id) do
    if socket.assigns.selected_role_id == removed_id do
      close_modals(socket)
    else
      socket
    end
  end

  defp close_modals(socket) do
    socket
    |> reset_form()
    |> assign(:selected_role_id, nil)
    |> assign(:selected_role, nil)
    |> assign(:selected_role_users, [])
    |> assign(:user_search_query, "")
    |> assign(:user_search_results, [])
    |> assign(:show_users_modal, false)
    |> assign_current_path(%{})
  end

  defp find_role(roles, id), do: Enum.find(roles, &(&1.id == id))

  defp extract_search_query(params) do
    params
    |> case do
      %{"q" => value} -> normalize_search_value(value)
      %{"search" => %{"q" => value}} -> normalize_search_value(value)
      %{"search" => search} when is_map(search) -> normalize_search_value(Map.get(search, "q"))
      _ -> ""
    end
  end

  defp normalize_search_value(nil), do: ""
  defp normalize_search_value(value) when is_binary(value), do: value
  defp normalize_search_value(value), do: to_string(value)

  defp users_modal_params(role_id, query) do
    base = %{"modal" => "users", "id" => role_id}

    case query do
      "" -> base
      _ -> Map.put(base, "email", query)
    end
  end

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

  defp assign_current_path(socket, params) when is_map(params) do
    assign(socket, :current_path, roles_path(params))
  end

  defp assign_current_path(socket, _params) do
    assign(socket, :current_path, roles_path(%{}))
  rescue
    _ -> assign(socket, :current_path, roles_path(%{}))
  end

  defp roles_path(params) do
    params
    |> Enum.reduce([], fn
      {key, value}, acc when value in [nil, ""] -> acc
      {key, value}, acc -> [{to_string(key), to_string(value)} | acc]
    end)
    |> Enum.reverse()
    |> case do
      [] -> "/admin/roles"
      query -> "/admin/roles?" <> URI.encode_query(query)
    end
  end
end
