defmodule WuiWeb.AdminUsersEditLive do
  use WuiWeb, :live_view
  import Ecto.Query
  require Logger
  # alias WuiWeb.Router.Helpers, as: Routes

  def mount(_params, _session, socket) do
    # if connected?(socket) do
    users_email =
      Wui.Repo.all(Wui.Accounts.User)
      |> Enum.map(fn user -> %{id: user.id, email: user.email} end)

    {
      :ok,
      socket
      |> assign(users: users_email)
      |> assign(selected_user: nil)
      |> assign(user_info: nil)
      |> assign(tab_show: :tab_user_info)
      # |> assign(debug: get_user_info(1).roles)
      #
      # |> assign(selected_user: 1)
      # |> assign(user_info: get_user_info(1))
      # |> assign(tab_show: :tab_roles)
      # |> assign(debug: Wui.Roles.all() -- get_user_roles(1))
      #
    }
  end

  def handle_params(params, _uri, socket) do
    par_user_id = params |> Map.get("id")

    selected_user =
      if par_user_id != nil and par_user_id != "" do
        case par_user_id |> Integer.parse() do
          {val, ""} -> val
          :error -> nil
          {_, _} -> nil
        end
      else
        nil
      end

    user_info = get_user_info(selected_user)

    socket =
      if user_info == nil and par_user_id != nil and par_user_id != "" do
        socket |> put_flash(:error, "Invalid user id #{inspect(par_user_id)} ")
      else
        socket
      end

    tab =
      case params |> Map.get("tab", "tab_user_info") do
        "tab_user_info" -> :tab_user_info
        "tab_roles" -> :tab_roles
        _ -> :tab_user_info
      end

    {
      :noreply,
      socket
      |> assign(debug: get_params_from_socket(socket))
      # |> assign(debug: socket.assigns)
      |> assign(selected_user: selected_user)
      |> assign(tab_show: tab)
      |> assign(user_info: get_user_info(selected_user))
      |> assign(from: params |> Map.get("from"))
      #
    }
  end

  defp get_user_roles(user_id) do
    user_roles_query = from(Wui.UsersRoles, where: [user_id: ^user_id])

    Wui.Repo.all(user_roles_query)
    |> Enum.map(fn ur -> ur.role_id end)
    |> Enum.map(fn role_id ->
      Wui.Roles.get_role(role_id)
    end)
  end

  defp get_user_info(user_id) do
    user_info =
      if user_id != nil and user_id != "None" do
        Wui.Accounts.User
        |> Wui.Repo.get(user_id)
      else
        nil
      end

    if user_info != nil do
      user_info
      |> Map.put(:roles, get_user_roles(user_id))
      |> Map.put(:possible_roles, Wui.Roles.all() -- get_user_roles(user_id))
    else
      nil
    end
  end

  defp get_params_from_socket(socket, params \\ []) do
    tab_show = socket.assigns.tab_show
    user_id = socket.assigns.selected_user
    from = socket.assigns |> Map.get(:from)

    "?" <>
      (params
       |> Enum.reduce(%{id: user_id, tab: tab_show, from: from}, fn {id, val}, acc ->
         acc |> Map.put(id, val)
       end)
       |> URI.encode_query())
  end

  def handle_event("user_updated", par, socket) do
    user_id = par |> Map.get("user_id")

    user_id =
      if user_id == "None" do
        nil
      else
        user_id |> String.to_integer()
      end

    # url_param = if user_id, do: "?" <> URI.encode_query(%{id: user_id}), else: ""
    url_params = get_params_from_socket(socket, id: user_id)

    {
      :noreply,
      socket
      |> push_patch(
        to: Routes.live_path(socket, WuiWeb.AdminUsersEditLive) <> url_params
      )
      # |> push_redirect(to: Routes.live_path(socket, WuiWeb.AdminUsersEditLive) <> url_param)
      # |> assign(debug: socket.assings)
      #
    }
  end

  def handle_event("switch-enabled-user", par, socket) do
    current = par |> Map.get("enabled-current", nil)
    id = par |> Map.get("id", "0") |> String.to_integer()

    new_val =
      if current == "false" do
        true
      else
        false
      end

    if current && id do
      Wui.Repo.get(Wui.Accounts.User, id)
      |> Ecto.Changeset.change(%{enabled: new_val})
      |> Wui.Repo.update()
    end

    {
      :noreply,
      socket
      |> assign(user_info: get_user_info(id))
      |> assign(selected_user: id)
    }
  end

  def handle_event("tab-show", par, socket) do
    tab = if par |> Map.get("tab") == "roles", do: :tab_roles, else: :tab_user_info

    url_params = get_params_from_socket(socket, tab: tab)

    {
      :noreply,
      socket
      |> push_patch(
        to: Routes.live_path(socket, WuiWeb.AdminUsersEditLive) <> url_params
      )

      # |> assign(tab_show: tab)
      # |> assign(debug: par)
    }
  end

  def handle_event("possible-role-click", par, socket) do
    user_id = par["user_id"] |> String.to_integer()
    role_id = par["role_id"] |> String.to_integer()

    change_set =
      %Wui.UsersRoles{}
      |> Wui.UsersRoles.changeset(%{
        user_id: user_id,
        role_id: role_id
      })

    {result, detail} = change_set |> Wui.Repo.insert()

    socket =
      socket
      # |> assign(debug: {result, inspect(detail.errors, pretty: true)})
      |> assign(user_info: get_user_info(user_id))

    socket =
      if result == :error do
        socket |> put_flash(:error, inspect(detail.errors, pretty: true))
      else
        socket
      end

    {
      :noreply,
      socket
    }
  end

  def handle_event("active-rol-click", par, socket) do
    user_id = par["user_id"] |> String.to_integer()
    role_id = par["role_id"] |> String.to_integer()

    from(Wui.UsersRoles, where: [user_id: ^user_id, role_id: ^role_id])
    |> Wui.Repo.delete_all()

    {
      :noreply,
      socket
      |> assign(user_info: get_user_info(user_id))
    }
  end

  def show_user_info(assigns) do
    ~H"""
    <table class="table is-size-5">
      <tbody>
        <tr>
          <th>enabled</th>
          <div class="block is-grouped">
          <td class={if @user_info.enabled == false, do: "px-6 has-text-danger", else: "px-6  has-text-success"}><%=@user_info.enabled%>
          <span class="button is-small"
                phx-click="switch-enabled-user"
                phx-value-enabled-current={inspect(@user_info.enabled)}
                phx-value-id={inspect(@user_info.id)}
                >
                <%= if @user_info.enabled, do: "deactivate", else: "activate"%>
          </span>
          </td>
          </div>
        </tr>
        <tr>
          <th>email</th><td class="px-6"><%=@user_info.email%></td>
        </tr>
        <tr>
          <th>confirmed at</th><td  class="px-6"><%=@user_info.confirmed_at%></td>
        </tr>
        <tr>
          <th>inserted at</th><td  class="px-6"><%=@user_info.inserted_at%></td>
        </tr>
        <tr>
          <th>updated at</th><td  class="px-6"><%=@user_info.updated_at%></td>
        </tr>
        <tr>
          <td>id</td><td  class="px-6"><%=@user_info.id%></td>
        </tr>
      </tbody>
    </table>
    """
  end
end
