defmodule Wui2Web.AdminUsersEdit do
  use Wui2Web, :live_view
  import Ecto.Query
  require Logger

  def mount(_params, _session, socket) do
    # if connected?(socket) do
    # users_email =
    #   Wui2.Repo.all(Wui2.Accounts.User)
    #   |> Enum.map(fn user -> %{id: user.id, email: user.email} end)

    {
      :ok,
      socket
      |> assign(tab_show: :tab_roles)
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
      case params |> Map.get("tab", "tab_roles") do
        "tab_user_info" -> :tab_user_info
        "tab_roles" -> :tab_roles
        _ -> :tab_roles
      end

    {
      :noreply,
      socket
      |> assign(debug: params)
      |> assign(tab_show: tab)
      |> assign(user_info: params["uid"] |> get_user_info())
      #
    }
  end

  defp socket_merge_params(socket, params \\ []) do
    tab_show = socket.assigns.tab_show

    "?" <>
      (params
       |> Enum.reduce(
         %{tab: tab_show},
         fn {id, val}, acc ->
           acc |> Map.put(id, val)
         end
       )
       |> URI.encode_query())
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
      Wui2.Repo.get(Wui2.Accounts.User, id)
      |> Ecto.Changeset.change(%{enabled: new_val})
      |> Wui2.Repo.update()
    end

    {
      :noreply,
      socket
      |> assign(user_info: get_user_info(id))
    }
  end

  def handle_event("tab-clicked", par, socket) do
    tab = if par |> Map.get("tab") == "user_info", do: :tab_user_info, else: :tab_roles
    url_params = socket |> socket_merge_params(tab: tab)

    {
      :noreply,
      socket
      |> push_patch(to: ~p"/admin/users/edit/#{socket.assigns.user_info.id}" <> url_params)
    }
  end

  def handle_event("possible-role-click", par, socket) do
    user_id = par["user_id"] |> String.to_integer()
    role_id = par["role_id"] |> String.to_integer()

    change_set =
      %Wui2.UsersRoles{}
      |> Wui2.UsersRoles.changeset(%{
        user_id: user_id,
        role_id: role_id
      })

    {result, detail} = change_set |> Wui2.Repo.insert()

    socket =
      socket
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

    from(Wui2.UsersRoles, where: [user_id: ^user_id, role_id: ^role_id])
    |> Wui2.Repo.delete_all()

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
            <td class={
              if @user_info.enabled == false,
                do: "px-6 has-text-danger",
                else: "px-6  has-text-success"
            }>
              <a
                phx-click="switch-enabled-user"
                phx-value-enabled-current={inspect(@user_info.enabled)}
                phx-value-id={inspect(@user_info.id)}
                type="checkbox"
                class="toggle toggle-success"
                checked={if @user_info.enabled, do: "true", else: "false"}
              />
            </td>
          </div>
        </tr>
        <tr>
          <th>email</th>
          <td class="px-6"><%= @user_info.email %></td>
        </tr>
        <tr>
          <th>confirmed at</th>
          <td class="px-6"><%= @user_info.confirmed_at %></td>
        </tr>
        <tr>
          <th>inserted at</th>
          <td class="px-6"><%= @user_info.inserted_at %></td>
        </tr>
        <tr>
          <th>updated at</th>
          <td class="px-6"><%= @user_info.updated_at %></td>
        </tr>
        <tr>
          <td>id</td>
          <td class="px-6"><%= @user_info.id %></td>
        </tr>
      </tbody>
    </table>
    """
  end

  defp get_user_roles(user_id) do
    user_roles_query = from(Wui2.UsersRoles, where: [user_id: ^user_id])

    Wui2.Repo.all(user_roles_query)
    |> Enum.map(fn ur -> ur.role_id end)
    |> Enum.map(fn role_id ->
      Wui2.Roles.get_role(role_id)
    end)
  end

  defp get_user_info(user_id) do
    user_info =
      if user_id != nil and user_id != "None" do
        Wui2.Accounts.User
        |> Wui2.Repo.get(user_id)
      else
        nil
      end

    if user_info != nil do
      user_info
      |> Map.put(:roles, get_user_roles(user_id))
      |> Map.put(:possible_roles, Wui2.Roles.all() -- get_user_roles(user_id))
    else
      nil
    end
  end
end
