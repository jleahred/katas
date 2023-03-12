defmodule Wui2Web.AdminUsersEdit do
  use Wui2Web, :live_view

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
    par_user_id = params |> Map.get("uid")

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

    tab =
      case params |> Map.get("tab", "tab_roles") do
        "tab_user_info" -> :tab_user_info
        "tab_roles" -> :tab_roles
        _ -> :tab_user_info
      end

    {
      :noreply,
      socket
      |> assign(tab_show: tab)
      |> assign(user_id: selected_user)
      #
    }
  end

  def render(assigns) do
    ~H"""
    <.live_component
      @socket
      module={Wui2Web.AdminUsersEditComponent}
      id="edit_user_comp"
      user_id={@user_id}
      tab_show={@tab_show}
      }
    />
    """
  end
end
