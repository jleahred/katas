defmodule Wui6Web.LiveAssignDefaults do
  @moduledoc """
  Provides default assigns for LiveViews, such as deriving the page title
  from the module metadata when available.
  """

  alias Phoenix.Component
  alias Wui6.Accounts
  alias Wui6.Accounts.Session
  alias Wui6.Accounts.User

  @doc false
  def on_mount(:default, _params, session, socket) do
    socket =
      socket
      |> maybe_assign_page_title()
      |> assign_current_session(session)

    {:cont, socket}
  end

  defp maybe_assign_page_title(socket) do
    Component.assign_new(socket, :page_title, fn -> metadata_title(socket.view) end)
  end

  defp metadata_title(nil), do: nil

  defp metadata_title(view_module) when is_atom(view_module) do
    if function_exported?(view_module, :metadata, 0) do
      view_module.metadata()[:title]
    else
      nil
    end
  end

  defp assign_current_session(socket, session) do
    token = current_token(session)
    current_session = current_session(session)
    current_user = current_session_user(current_session)

    current_email =
      case current_user do
        %User{email: email} -> email
        _ -> Map.get(session, "current_session_email") || Map.get(session, :current_session_email)
      end

    current_roles =
      case current_user do
        %User{roles: roles} when is_list(roles) ->
          Enum.map(roles, &role_to_payload/1)

        _ ->
          Map.get(session, "current_session_roles") || Map.get(session, :current_session_roles) ||
            []
      end

    socket
    |> Component.assign(:register_token, token)
    |> Component.assign(:current_session, current_session)
    |> Component.assign(:current_session_user, current_user)
    |> Component.assign(:current_user_email, current_email)
    |> Component.assign(:current_user_roles, current_roles)
  end

  defp current_token(session) do
    Map.get(session, "wui6_token") || Map.get(session, :wui6_token)
  end

  defp current_session(session) do
    case current_token(session) do
      token when is_binary(token) and token != "" ->
        Accounts.get_session_with_user_by_token(token)

      _ ->
        nil
    end
  end

  defp current_session_user(%Session{user: %User{} = user}), do: user
  defp current_session_user(_), do: nil

  defp role_to_payload(%{id: id, name: name}), do: %{id: id, name: name}
  defp role_to_payload(_), do: %{}
end
