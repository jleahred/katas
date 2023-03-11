defmodule Wui2Web.AdminUsersRenderUsers do
  use Wui2Web, :live_view
  # import Wui2Web.AdminUsersParams
  # alias Wui2Web.AdminUsersParams.Params, as: Params
  # require Logger
  # alias Wui2Web.Router.Helpers, as: Routes

  def users_table(assigns) do
    ~H"""
    <table class="uk-table uk-table-striped">
      <tbody>
        <tr>
          <th>id</th>
          <th>email</th>
          <th>enabled</th>
          <th>confirmed_at</th>
          <th>roles</th>
          <th></th>
        </tr>
        <%= for r <- @records do %>
          <tr>
            <td class="uk-table-shrink"><%= r.id %></td>
            <td class="uk-table-shrink"><%= r.email %></td>
            <td class="uk-table-shrink">
              <a
                phx-click="switch-enabled-user"
                phx-value-enabled-current={inspect(r.enabled)}
                phx-value-id={inspect(r.id)}
                type="checkbox"
                class="toggle toggle-success"
                checked={if r.enabled, do: "true", else: "false"}
              />
            </td>
            <td class="uk-text-nowrap uk-table-shrink"><%= r.confirmed_at %></td>
            <td class="uk-text-truncate">
              <%= for role_name <- r.role_names do %>
                <%= "#{role_name} · " %>
              <% end %>
            </td>
            <td class="uk-text-nowrap">
              <a href={~p"/admin/users/edit/" <> "#{r.id}"}>
                <Heroicons.pencil_square class="w-6" />
              </a>
            </td>
          </tr>
        <% end %>
      </tbody>
    </table>
    """
  end

  def render(assigns), do: ~H""
end
