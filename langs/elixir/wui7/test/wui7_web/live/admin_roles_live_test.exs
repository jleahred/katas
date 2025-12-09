defmodule Wui7Web.AdminRolesLiveTest do
  use Wui7Web.ConnCase

  import Phoenix.LiveViewTest

  alias Wui7.Accounts

  test "redirects guests to login" do
    conn = build_conn()

    assert {:error, {:redirect, %{to: path}}} = live(conn, ~p"/admin/roles")
    assert path == ~p"/users/log-in"
  end

  test "lists, creates, updates and deletes roles", %{conn: conn} do
    %{conn: conn} = register_and_log_in_user(%{conn: conn})
    {:ok, role} = Accounts.create_role(%{code: "admin", description: "Administrador"})

    {:ok, view, html} = live(conn, ~p"/admin/roles")
    assert html =~ "Administrador"

    view
    |> element("button[phx-click=\"new-role\"]")
    |> render_click()

    view
    |> form("#role-form", %{"role" => %{"code" => "user", "description" => "Básico"}})
    |> render_submit()

    assert render(view) =~ "Rol creado"
    assert render(view) =~ "user"

    view
    |> element("button[phx-click=\"edit-role\"][phx-value-id=\"#{role.id}\"]")
    |> render_click()

    view
    |> form("#role-form", %{"role" => %{"code" => "admin", "description" => "Actualizado"}})
    |> render_submit()

    assert render(view) =~ "Rol actualizado"
    assert render(view) =~ "Actualizado"

    view
    |> element("button[phx-click=\"delete-role\"][phx-value-id=\"#{role.id}\"]")
    |> render_click()

    refute render(view) =~ "Administrador"
  end
end
