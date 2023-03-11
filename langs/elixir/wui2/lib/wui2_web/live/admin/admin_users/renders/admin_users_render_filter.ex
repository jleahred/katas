defmodule Wui2Web.AdminUsersRenderFilter do
  use Wui2Web, :live_view

  # alias Wui2Web.Router.Helpers, as: Routes

  def filter_form(assigns) do
    ~H"""
    <div class="">
      <label class="label">
        <span class="label-text">Filter</span>
      </label>
      <form id="fuser" phx-change="filter_form_updated">
        <div class="grid grid-cols-2 gap-2">
          <input
            class="input input-bordered w-full"
            type="search"
            placeholder="email"
            id="email"
            name="email"
            value={@params.email}
            phx-debounce="500"
          />

          <select class="select select-bordered" name="role_id" id="role_id">
            <option class="" value="all" selected>All roles</option>
            <%= for role <- @roles do %>
              <option class="" value={role.id} selected={@params.role_id == role.id}>
                <%= "#{role.name}" %>
              </option>
            <% end %>
          </select>
        </div>
      </form>
    </div>
    """
  end

  def render(assigns), do: ~H""
end
