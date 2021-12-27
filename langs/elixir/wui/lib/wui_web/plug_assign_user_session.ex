defmodule WuiWeb.PlugAssignUserSession do
  import Plug.Conn

  alias Wui.Accounts

  @remember_me_cookie "_wui_web_user_remember_me"

  def init(opts), do: opts

  def call(conn, _params) do
    {user_token, conn} = ensure_user_token(conn)
    user = user_token && Accounts.get_user_by_session_token(user_token)

    if user do
      put_session(conn, "current_user", user)
    else
      conn
    end
  end

  defp ensure_user_token(conn) do
    if user_token = get_session(conn, :user_token) do
      {user_token, conn}
    else
      conn = fetch_cookies(conn, signed: [@remember_me_cookie])

      if user_token = conn.cookies[@remember_me_cookie] do
        {user_token, put_session(conn, :user_token, user_token)}
      else
        {nil, conn}
      end
    end
  end
end
