defmodule Wui2Web.AdminUsers do
  use Wui2Web, :live_view

  import Wui2Web.AdminUsersParams
  import Wui2Web.AdminUsersRenderFilter
  import Wui2Web.AdminUsersRenderUsers

  alias Wui2Web.AdminUsersParams.Params, as: Params
  import Ecto.Query

  require Logger

  # alias Wui2Web.Router.Helpers, as: Routes

  def mount(_params, _session, socket) do
    # socket =
    #   if connected?(socket) do
    #     socket
    #     |> assign(roles: Wui2.Roles.all())
    #   else
    #     socket
    #     |> assign(roles: %{})
    #   end

    {
      :ok,
      socket
      |> assign(roles: Wui2.Roles.all())
      |> assign(editting_user_id: nil)
      # |> assign(selected_role: 0)
      # |> assign(params: %Params{})
      # |> assign(debug: %Params{})
      #
    }
  end

  def get_role_names_for_user(user_id, userid2roles) do
    userid2roles
    |> Map.get(user_id, [])
    |> Enum.sort()
    |> Enum.map(&Wui2.Roles.get_role(&1).name)
  end

  def handle_params(params, _uri, socket) do
    params = normalize_params(params)

    {
      :noreply,
      socket
      # |> assign(debug: get_records_from_params(params))
      |> assign(debug: nil)
      |> assign(records: get_records_from_params(params))
      |> assign(params: params)
      |> assign(editting_user_id: params.editting_user_id)
      #
    }
  end

  def handle_event("close_user_edit", _par, socket) do
    socket_par =
      get_current_params_from_socket(socket)
      |> Map.put(:editting_user_id, nil)

    Logger.info("=========== #{inspect(socket_par)}")

    url_params = socket_par |> params_to_url()

    {
      :noreply,
      socket
      |> assign(editting_user_id: nil)
      |> push_patch(to: ~p"/admin/users" <> url_params)
    }
  end

  def handle_event("filter_form_updated", par, socket) do
    par =
      get_current_params_from_socket(socket)
      |> merge_params(normalize_params(par))

    url_params = par |> params_to_url()

    {
      :noreply,
      socket
      |> push_patch(to: ~p"/admin/users" <> url_params)
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
      Wui2.Repo.get(Wui2.Accounts.User, id)
      |> Ecto.Changeset.change(%{enabled: new_val})
      |> Wui2.Repo.update()
    end

    {
      :noreply,
      socket
      |> assign(
        records:
          socket
          |> get_current_params_from_socket()
          |> get_records_from_params()
      )
    }
  end

  def handle_event("edit_user", par, socket) do
    socket_par = get_current_params_from_socket(socket)

    socket_par =
      if par["uid"],
        do: %{socket_par | editting_user_id: par["uid"]},
        else: socket_par

    url_params = socket_par |> params_to_url()

    {
      :noreply,
      socket
      |> push_patch(to: ~p"/admin/users" <> url_params)
      |> assign(editting_user_id: par["uid"])
    }

    # {
    #   :noreply,
    #   socket
    #   |> assign(editting_user_id: par["uid"])
    # }
  end

  defp get_current_params_from_socket(socket) do
    socket.assigns |> Map.get(:params, %Params{})
  end

  defp get_records_from_params(params) do
    filter_email = if params.email, do: "%#{params.email}%", else: "%"

    # get from db users_id filter by email if so
    users_id_filter_email =
      from(users in Wui2.Accounts.User,
        where: like(users.email, ^filter_email),
        select: users.id
      )
      |> Wui2.Repo.all()

    # get from db users_roles the users_id filter by selected role if so
    usersids_filter_by_role =
      if params.role_id do
        from(ur in Wui2.UsersRoles,
          where: ur.role_id == ^params.role_id,
          select: ur.user_id
        )
        |> Wui2.Repo.all()
      else
        []
      end

    #
    map_userid2rolesid =
      if users_id_filter_email != [] do
        from(ur in Wui2.UsersRoles,
          where: ur.user_id in ^users_id_filter_email,
          select: [ur.user_id, ur.role_id]
        )
        |> Wui2.Repo.all()
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

    from(users in Wui2.Accounts.User,
      where: users.id in ^user_ids
    )
    |> Wui2.Repo.all()
    |> Enum.map(&Map.from_struct/1)
    |> Enum.map(
      &(&1
        |> Map.put(:role_names, get_role_names_for_user(&1.id, map_userid2rolesid)))
    )
  end
end
