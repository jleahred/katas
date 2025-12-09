defmodule Wui7Web.SearchLiveTest do
  use Wui7Web.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "muestra los resultados iniciales", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    assert has_element?(view, "#result-counter")
    assert has_element?(view, "#result-counter_cp2")
  end

  test "filtra y ordena por coincidencias", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> form("#search-form", search: %{"query" => "doble estadísticas"})
    |> render_change()

    html = render(view)
    assert {idx_counter2, _} = :binary.match(html, "result-counter2")
    assert {idx_counter_cp2, _} = :binary.match(html, "result-counter_cp2")
    assert idx_counter2 < idx_counter_cp2
  end
end
