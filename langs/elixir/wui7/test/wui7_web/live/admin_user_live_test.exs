defmodule Wui7Web.AdminUserLiveTest do
  use Wui7Web.ConnCase

  import Phoenix.LiveViewTest
  import Wui7.AccountsFixtures

  alias Wui7.Accounts

  test "redirects guests to log in" do
    conn = build_conn()

    assert {:error, {:redirect, %{to: path}}} = live(conn, ~p"/admin/user/1")
    assert path == ~p"/users/log-in"
  end

  test "shows the user information and tokens", %{conn: conn} do
    %{conn: conn} = register_and_log_in_user(%{conn: conn})
    target_user = user_fixture()
    Accounts.generate_user_session_token(target_user)

    {:ok, _view, html} = live(conn, ~p"/admin/user/#{target_user.id}")

    assert html =~ target_user.email
    assert html =~ "Detalle de usuario"
    assert html =~ "No configurada"

    [token | _] = Accounts.list_user_tokens(target_user)
    encoded = Base.encode64(token.token)
    assert html =~ encoded
    assert html =~ target_user.id |> Integer.to_string()
  end

  test "allows toggling enabled state", %{conn: conn} do
    %{conn: conn} = register_and_log_in_user(%{conn: conn})
    target_user = user_fixture()

    {:ok, view, _} = live(conn, ~p"/admin/user/#{target_user.id}")

    assert render(view) =~ "Deshabilitado"

    view |> element("#user-enabled-toggle") |> render_click()

    assert render(view) =~ "Habilitado"
  end
end
