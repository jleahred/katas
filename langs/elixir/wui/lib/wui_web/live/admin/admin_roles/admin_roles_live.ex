defmodule WuiWeb.AdminRolesLive do
  use WuiWeb, :live_view
  # alias WuiWeb.Router.Helpers, as: Routes

  def mount(_params, _session, socket) do
    # if connected?(socket) do

    {
      :ok,
      socket
      |> assign(debug: nil)
      |> assign(roles: Wui.Roles.all())
      #
    }
  end
end
