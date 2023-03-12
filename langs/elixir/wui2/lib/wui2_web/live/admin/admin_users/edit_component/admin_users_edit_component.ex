defmodule Wui2Web.AdminUsersEditComponent do
  use Wui2Web, :live_view
  use Phoenix.LiveComponent
  import Ecto.Query

  require Logger

  def mount(socket) do
    # if connected?(socket) do
    # users_email =
    #   Wui2.Repo.all(Wui2.Accounts.User)
    #   |> Enum.map(fn user -> %{id: user.id, email: user.email} end)

    {
      :ok,
      socket
      #
    }
  end

  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(user_info: assigns.user_id |> get_user_info())
     |> assign(user_id: assigns.user_id)
     |> assign(tab_show: assigns.tab_show)}
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
    {
      :noreply,
      socket
      |> assign(tab_show: if(par["tab"] == "user_info", do: :tab_user_info, else: :tab_roles))
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
                phx-target={@myself}
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

  def render(assigns) do
    ~H"""
    <div class="grid place-items-center">
      <%= if(@user_info) do %>
        <.header>
          User: <%= @user_info.email %>
        </.header>
        <div class="tabs">
          <span
            class={"tab tab-bordered #{if @tab_show == :tab_user_info, do: "tab-active"} "}
            phx-target={@myself}
            phx-click="tab-clicked"
            phx-value-tab="user_info"
          >
            Info
          </span>
          <span
            class={"tab tab-bordered #{if @tab_show == :tab_roles, do: "tab-active"} "}
            phx-target={@myself}
            phx-click="tab-clicked"
            phx-value-tab="roles"
          >
            Roles
          </span>
        </div>

        <%= if @tab_show == :tab_user_info do %>
          <.show_user_info user_info={@user_info} myself={@myself} />
        <% end %>

        <%= if @tab_show == :tab_roles  do %>
          <div class="grid grid-cols-2 gap-8  py-8">
            <div>
              Active roles
              <ul class="container box-content  h-48 overflow-auto border rounded border-slate-300">
                <%= for role <- @user_info.roles do %>
                  <li>
                    <span
                      class="button is-link is-inverted is-fullwidth"
                      phx-target={@myself}
                      phx-click="active-rol-click"
                      phx-value-user_id={@user_info.id}
                      phx-value-role_id={role.id}
                    >
                      <div class="badge badge-success gap-2">
                        <%= role.name %>
                        <Heroicons.x_mark class="w-5 h-4" />
                      </div>
                    </span>
                  </li>
                <% end %>
              </ul>
            </div>
            <div>
              Possible roles
              <ul class="container box-content  h-48 overflow-auto border rounded border-slate-300">
                <%= for role <- @user_info.possible_roles do %>
                  <li>
                    <span
                      class="button is-link is-inverted is-fullwidth"
                      phx-target={@myself}
                      phx-click="possible-role-click"
                      phx-value-user_id={@user_info.id}
                      phx-value-role_id={role.id}
                    >
                      <div class="badge badge-error gap-2">
                        <Heroicons.plus_small class="w-5 h-6" />
                        <%= role.name %>
                      </div>
                    </span>
                  </li>
                <% end %>
              </ul>
            </div>
          </div>
        <% end %>
      <% else %>
        User not found :-( <%= inspect(@user_id) %>
      <% end %>
    </div>
    """
  end
end
