defmodule Wui6Web.Plugs.RegisterSession do
  @moduledoc """
  Reads the signed session token cookie, refreshes metadata and exposes the current session.
  """

  import Plug.Conn

  alias Wui6.Accounts
  alias Wui6Web.RequestContext

  @cookie "wui6_token"

  def init(opts), do: opts

  def call(conn, _opts) do
    conn = fetch_cookies(conn, signed: [@cookie])
    token = conn.cookies[@cookie]

    cond do
      is_binary(token) and token != "" ->
        snapshot = RequestContext.snapshot(conn)

        case Accounts.refresh_session_snapshot(token, snapshot) do
          {:ok, session} ->
            conn
            |> assign(:current_session, session)
            |> assign(:current_user, session.user)
            |> assign(:current_user_roles, session.user.roles || [])
            |> put_session(:wui6_token, token)
            |> put_session(:current_session_email, session.user.email)
            |> put_session(
              :current_session_roles,
              (session.user.roles || [])
              |> Enum.map(&role_payload/1)
              |> Enum.reject(&(&1 == %{}))
            )

          {:error, _reason} ->
            conn
            |> clear_session_token()
        end

      true ->
        conn
        |> clear_session_token()
    end
  end

  defp clear_session_token(conn) do
    conn
    |> delete_resp_cookie(@cookie, cookie_opts(conn))
    |> delete_session(:wui6_token)
    |> delete_session(:current_session_email)
    |> delete_session(:current_session_roles)
    |> assign(:current_session, nil)
    |> assign(:current_user, nil)
    |> assign(:current_user_roles, [])
  end

  defp cookie_opts(conn) do
    [
      sign: true,
      http_only: true,
      same_site: "Strict",
      secure: conn.scheme == :https,
      max_age: 0
    ]
  end

  defp role_payload(%{id: id, name: name}) do
    %{id: id, name: name}
  end

  defp role_payload(_), do: %{}
end
