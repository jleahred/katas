defmodule Wui2Web.AdminShowUsersRenders do
  use Wui2Web, :live_view
  # import Wui2Web.AdminShowUsersParams
  # alias Wui2Web.AdminShowUsersParams.Params, as: Params
  # require Logger
  # alias Wui2Web.Router.Helpers, as: Routes

  def filter_form(assigns) do
    ~H"""
    <div class="block">
      <div class="columns is-centered">
        <div class="">
          <form id="fuser" phx-change="params_updated">
            <div class="field is-grouped">
              <input
                class="input"
                type="search"
                placeholder="email"
                id="email"
                name="email"
                value={@params.email}
                phx-debounce="500"
              />

              <div class="field">
                <div class="select">
                  <select name="role_id" id="role_id" class="">
                    <option class="has-text-left" value="all" selected>All roles</option>
                    <%= for role <- @roles do %>
                      <option
                        class="has-text-left"
                        value={role.id}
                        selected={@params.role_id == role.id}
                      >
                        <%= "#{role.name}" %>
                      </option>
                    <% end %>
                  </select>
                </div>
              </div>
            </div>
          </form>
        </div>
      </div>
    </div>
    """
  end

  def render(assigns), do: ~H""
end
