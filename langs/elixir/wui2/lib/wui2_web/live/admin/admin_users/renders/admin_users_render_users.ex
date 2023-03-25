defmodule Wui2Web.AdminUsersRenderUsers do
  use Wui2Web, :live_view
  # import Wui2Web.AdminUsersParams
  # alias Wui2Web.AdminUsersParams.Params, as: Params
  # require Logger
  # alias Wui2Web.Router.Helpers, as: Routes

  def users_table(assigns) do
    ~H"""
    <.wtable id="users" rows={@records}>
      <:col :let={user} label="id"><%= user.id %></:col>
      <:col :let={user} label="email"><%= user.email %></:col>
      <:col :let={user} label="enabled">
        <a
          phx-click="switch-enabled-user"
          phx-value-enabled-current={inspect(user.enabled)}
          phx-value-id={inspect(user.id)}
          type="checkbox"
          class="toggle toggle-success"
          checked={if user.enabled, do: "true", else: "false"}
        />
      </:col>
      <:col :let={user} label="confirmed_at"><%= user.confirmed_at %></:col>
      <:col :let={user} label="created"><%= user.inserted_at %></:col>
      <:col :let={user} label="updated"><%= user.updated_at %></:col>

      <:action :let={user}>
        <Heroicons.pencil_square class="w-6" phx-click="edit_user" phx-value-uid={user.id} />
      </:action>
    </.wtable>
    """
  end
end
