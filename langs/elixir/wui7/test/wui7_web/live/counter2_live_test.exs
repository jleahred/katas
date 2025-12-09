defmodule Wui7Web.Counter2LiveTest do
  use Wui7Web.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "lee ambos contadores desde la URL", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter2?count_a=3&count_b=-2")

    assert has_element?(view, "#counter-a-value", "3")
    assert has_element?(view, "#counter-b-value", "-2")
  end

  test "ajustar el contador A sincroniza la url", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter2?count_a=1&count_b=5")

    view |> element("#counter-a-inc-1") |> render_click()

    assert_patch(view, ~p"/counter2?count_a=2&count_b=5")
    assert has_element?(view, "#counter-a-value", "2")
  end

  test "el formulario manual actualiza ambos valores", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter2")

    view
    |> form("#pair-form", pair: %{"count_a" => "10", "count_b" => "-7"})
    |> render_submit()

    assert_patch(view, ~p"/counter2?count_a=10&count_b=-7")
    assert has_element?(view, "#counter-a-value", "10")
    assert has_element?(view, "#counter-b-value", "-7")
  end
end
