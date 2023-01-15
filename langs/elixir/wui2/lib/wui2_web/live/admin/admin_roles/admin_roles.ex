defmodule Wui2Web.AdminRoles do
  use Wui2Web, :live_view
  # alias Wui2Web.Router.Helpers, as: Routes

  def mount(_params, _session, socket) do
    # if connected?(socket) do

    {
      :ok,
      socket
      |> assign(debug: nil)
      |> assign(roles: Wui2.Roles.all())
      #
    }
  end
end
