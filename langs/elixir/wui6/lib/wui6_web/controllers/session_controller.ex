defmodule Wui6Web.SessionController do
  use Wui6Web, :controller

  alias Wui6.Accounts

  @cookie "wui6_token"

  def logout(conn, _params) do
    conn = fetch_cookies(conn, signed: [@cookie])

    conn.cookies
    |> Map.get(@cookie)
    |> Accounts.delete_session_by_token()
    |> case do
      :ok -> :ok
      {:error, _reason} -> :error
      _ -> :ok
    end

    conn
    |> clear_token_cookie()
    |> clear_session()
    |> put_flash(:info, "Has cerrado la sesión correctamente.")
    |> redirect(to: ~p"/")
  end

  defp clear_token_cookie(conn) do
    conn
    |> delete_resp_cookie(@cookie, cookie_opts(conn))
    |> delete_session(:wui6_token)
    |> delete_session(:current_session_email)
    |> delete_session(:current_session_roles)
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
end
