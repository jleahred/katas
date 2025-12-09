defmodule Wui7Web.AdminUsersLiveTest do
  use Wui7Web.ConnCase

  import Phoenix.LiveViewTest
  import Wui7.AccountsFixtures

  test "redirects guests to log in" do
    conn = build_conn()

    assert {:error, {:redirect, %{to: path}}} = live(conn, ~p"/admin/users")
    assert path == ~p"/users/log-in"
  end

  test "lists users for authenticated visitor", %{conn: conn} do
    %{conn: conn, user: user} = register_and_log_in_user(%{conn: conn})
    other_user = user_fixture()

    {:ok, _view, html} = live(conn, ~p"/admin/users")

    assert html =~ user.email
    assert html =~ other_user.email
    assert html =~ "Usuarios"
  end
end
