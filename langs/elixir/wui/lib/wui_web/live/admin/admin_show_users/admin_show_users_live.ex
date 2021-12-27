defmodule WuiWeb.AdminShowUsersLive do
  use WuiWeb, :live_view
  import WuiWeb.AdminShowUsersParams
  import WuiWeb.AdminShowUsersRenders
  alias WuiWeb.AdminShowUsersParams.Params, as: Params
  import Ecto.Query

  require Logger
  # alias WuiWeb.Router.Helpers, as: Routes

  def mount(_params, _session, socket) do
    # socket =
    #   if connected?(socket) do
    #     socket
    #     |> assign(roles: Wui.Roles.all())
    #   else
    #     socket
    #     |> assign(roles: %{})
    #   end

    {
      :ok,
      socket
      |> assign(roles: Wui.Roles.all())
      # |> assign(selected_role: 0)
      # |> assign(params: %Params{})
      # |> assign(debug: %Params{})
      #
    }
  end

  def get_records_form_params(params) do
    filter_email = if params.email, do: "%#{params.email}%", else: "%"

    # get from db users_id filter by email if so
    users_id_filter_email =
      from(users in Wui.Accounts.User,
        where: like(users.email, ^filter_email),
        select: users.id
      )
      |> Wui.Repo.all()

    # get from db users_roles the users_id filter by selected role if so
    usersids_filter_by_role =
      if params.role_id do
        from(ur in Wui.UsersRoles,
          where: ur.role_id == ^params.role_id,
          select: ur.user_id
        )
        |> Wui.Repo.all()
      else
        []
      end

    #
    map_userid2rolesid =
      if users_id_filter_email != [] do
        from(ur in Wui.UsersRoles,
          where: ur.user_id in ^users_id_filter_email,
          select: [ur.user_id, ur.role_id]
        )
        |> Wui.Repo.all()
      else
        []
      end
      |> Enum.reduce(%{}, fn [uid, roleid], acc ->
        acc |> Map.update(uid, [roleid], fn c -> [roleid | c] end)
      end)

    # filter users_id that are in both sets
    user_ids =
      if usersids_filter_by_role != [] do
        usersids_filter_by_role
        |> Enum.filter(&(users_id_filter_email |> Enum.member?(&1)))
      else
        users_id_filter_email
      end

    # get all roles from users_ids

    from(users in Wui.Accounts.User,
      where: users.id in ^user_ids
    )
    |> Wui.Repo.all()
    |> Enum.map(&Map.from_struct/1)
    |> Enum.map(
      &(&1
        |> Map.put(:role_names, get_role_names_for_user(&1.id, map_userid2rolesid)))
    )
  end

  def get_role_names_for_user(user_id, userid2roles) do
    userid2roles
    |> Map.get(user_id, [])
    |> Enum.sort()
    |> Enum.map(&Wui.Roles.get_role(&1).name)
  end

  def handle_params(params, _uri, socket) do
    params = normalize_params(params)

    from =
      "#{Routes.live_path(socket, WuiWeb.AdminShowUsersLive)}?#{params |> Map.from_struct() |> URI.encode_query()}"

    {
      :noreply,
      socket
      # |> assign(debug: get_records_form_params(params))
      |> assign(debug: nil)
      |> assign(records: get_records_form_params(params))
      |> assign(params: params)
      |> assign(from: from)
      #
    }
  end

  def handle_event("params_updated", par, socket) do
    par =
      get_current_params_from_socket(socket)
      |> merge_params(normalize_params(par))

    url_params = par |> params_to_url()

    {
      :noreply,
      socket
      |> push_patch(
        to: Routes.live_path(socket, WuiWeb.AdminShowUsersLive) <> url_params
      )
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
      |> assign(
        records: socket |> get_current_params_from_socket() |> get_records_form_params()
      )
    }
  end

  defp get_current_params_from_socket(socket) do
    socket.assigns |> Map.get(:params, %Params{})
  end
end
