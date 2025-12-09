defmodule Wui7Web.TestLiveTest do
  use Wui7Web.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "shows diagnostic information for guests", %{conn: conn} do
    {:ok, _view, html} = live(conn, ~p"/test")

    assert html =~ "Panel de diagnóstico"
    assert html =~ "Invitado"
    assert html =~ "Datos del cliente"
  end

  test "shows authenticated user data", %{conn: conn} do
    %{conn: conn, user: user} = register_and_log_in_user(%{conn: conn})
    {:ok, _view, html} = live(conn, ~p"/test")

    assert html =~ user.email
    assert html =~ "Usuario autenticado"
  end
end
