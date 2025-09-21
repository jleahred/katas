defmodule Wui6Web.AdminUsersLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts
  alias Wui6.Roles

  @metadata %{
    title: "Admin · Users",
    description: "Listado de usuarios con fecha de creación y estado de habilitación.",
    keywords: ["admin", "users", "accounts"]
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    limit = 10

    {:ok,
     socket
     |> assign(:limit, limit)
     |> assign(:users, [])
     |> assign(:has_more?, false)
     |> assign(:next_page, nil)
     |> assign(:prev_page, nil)
     |> assign(:page, 1)
     |> assign(:query, "")
     |> assign(:role_filter, nil)
     |> assign(:roles, Roles.list_roles())
     |> assign(:current_path, ~p"/admin/users")
     |> assign(:page_title, "Usuarios")}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    query =
      params
      |> Map.get("q", "")
      |> to_string()
      |> String.trim()

    page = params |> Map.get("page") |> parse_page()
    role_filter = params |> Map.get("role") |> parse_role()

    page_data =
      Accounts.list_users(
        limit: socket.assigns.limit,
        page: page,
        query: query,
        role_id: role_filter
      )

    {:noreply, assign_page(socket, page_data, query, role_filter)}
  end

  @impl true
  def handle_event("filter", params, socket) do
    trimmed = params |> Map.get("q", "") |> String.trim()

    {role_value, role_changed?} =
      case Map.fetch(params, "role") do
        {:ok, value} -> {value, role_param(value) != socket.assigns.role_filter}
        :error -> {socket.assigns.role_filter, false}
      end

    query_changed? = trimmed != socket.assigns.query

    {trimmed, role_param} =
      cond do
        role_changed? ->
          {"", role_value}

        query_changed? ->
          {trimmed, nil}

        true ->
          {trimmed, role_value}
      end

    {:noreply, push_patch(socket, to: users_path(trimmed, 1, role_param))}
  end

  @impl true
  def handle_event("next_page", _params, %{assigns: %{next_page: nil}} = socket) do
    {:noreply, socket}
  end

  def handle_event("next_page", _params, socket) do
    {:noreply,
     push_patch(socket,
       to:
         users_path(
           socket.assigns.query,
           socket.assigns.next_page,
           socket.assigns.role_filter
         )
     )}
  end

  @impl true
  def handle_event("prev_page", _params, %{assigns: %{prev_page: nil}} = socket) do
    {:noreply, socket}
  end

  def handle_event("prev_page", _params, socket) do
    {:noreply,
     push_patch(socket,
       to:
         users_path(
           socket.assigns.query,
           socket.assigns.prev_page,
           socket.assigns.role_filter
         )
     )}
  end

  @impl true
  def handle_event("toggle_enabled", %{"id" => id}, socket) do
    with {user_id, ""} <- Integer.parse(id),
         %{} = current_user <- find_user(socket.assigns.users, user_id) do
      case Accounts.toggle_user_enabled(user_id) do
        {:ok, _updated_user} ->
          {:noreply, reload_current_page(socket)}

        {:error, _reason} ->
          updated_user = toggle_enabled_struct(current_user)
          {:noreply, assign(socket, users: upsert_user(socket.assigns.users, updated_user))}
      end
    else
      _ -> {:noreply, socket}
    end
  end

  defp reload_current_page(socket) do
    page_data =
      Accounts.list_users(
        limit: socket.assigns.limit,
        page: socket.assigns.page,
        query: socket.assigns.query,
        role_id: socket.assigns.role_filter
      )

    assign_page(socket, page_data, socket.assigns.query, socket.assigns.role_filter)
  end

  defp assign_page(socket, page_data, query, role_filter) do
    socket
    |> assign(:users, page_data.entries)
    |> assign(:has_more?, page_data.has_more?)
    |> assign(:next_page, page_data.next_page)
    |> assign(:prev_page, page_data.prev_page)
    |> assign(:page, page_data.page)
    |> assign(:query, query)
    |> assign(:role_filter, role_filter)
    |> assign(:current_path, users_path(query, page_data.page, role_filter))
  end

  defp users_path(query, page, role_filter \\ nil) do
    params = []
    params = maybe_put_param(params, :q, blank_to_nil(query))
    params = maybe_put_param(params, :page, page_param(page))
    params = maybe_put_param(params, :role, role_param(role_filter))

    case params do
      [] -> ~p"/admin/users"
      _ -> ~p"/admin/users?#{params}"
    end
  end

  defp maybe_put_param(params, _key, nil), do: params
  defp maybe_put_param(params, key, value), do: Keyword.put(params, key, value)

  defp blank_to_nil(nil), do: nil
  defp blank_to_nil(""), do: nil
  defp blank_to_nil(value), do: value

  defp page_param(nil), do: nil
  defp page_param(page) when page <= 1, do: nil
  defp page_param(page), do: page

  defp parse_page(nil), do: 1

  defp parse_page(value) do
    case Integer.parse(to_string(value)) do
      {int, ""} when int > 0 -> int
      _ -> 1
    end
  end

  defp role_param(nil), do: nil
  defp role_param(role) when role in ["", :none], do: nil
  defp role_param(role) when is_integer(role) and role > 0, do: role

  defp role_param(role) when is_binary(role) do
    case Integer.parse(role) do
      {int, ""} when int > 0 -> int
      _ -> nil
    end
  end

  defp role_param(_), do: nil

  defp parse_role(nil), do: nil

  defp parse_role(value) do
    value
    |> to_string()
    |> Integer.parse()
    |> case do
      {int, ""} when int > 0 -> int
      _ -> nil
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

  defp find_user(users, user_id) do
    Enum.find(users, fn user -> user.id == user_id end)
  end

  defp upsert_user(users, updated_user) do
    Enum.map(users, fn user ->
      if user.id == updated_user.id, do: updated_user, else: user
    end)
  end

  defp toggle_enabled_struct(%_{} = user) do
    struct(user, enabled: not user_enabled?(user))
  end

  defp toggle_enabled_struct(user) when is_map(user) do
    Map.put(user, :enabled, not user_enabled?(user))
  end

  defp user_enabled?(user), do: Map.get(user, :enabled, true)

  defp row_text_class(user) do
    if user_enabled?(user), do: "", else: "text-base-content/60"
  end

  defp row_classes(user) do
    base = ["transition-colors"]

    base =
      if user.id == 1 do
        ["bg-error/12" | base]
      else
        base
      end

    Enum.join(base, " ")
  end
end
