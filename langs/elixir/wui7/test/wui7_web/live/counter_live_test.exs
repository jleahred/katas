defmodule Wui7Web.CounterLiveTest do
  use Wui7Web.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "carga el valor desde la URL", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter?count=7")

    assert has_element?(view, "#counter-value", "7")
  end

  test "incrementar sincroniza el query string", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter?count=2")

    view |> element("#increment-btn") |> render_click()

    assert_patch(view, ~p"/counter?count=3")
    assert has_element?(view, "#counter-value", "3")
  end

  test "parámetros no válidos usan el valor por defecto", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter?count=oops")

    assert has_element?(view, "#counter-value", "0")
  end
end
