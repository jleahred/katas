defmodule Wui2Web.ErrorJSONTest do
  use Wui2Web.ConnCase, async: true

  test "renders 404" do
    assert Wui2Web.ErrorJSON.render("404.json", %{}) == %{errors: %{detail: "Not Found"}}
  end

  test "renders 500" do
    assert Wui2Web.ErrorJSON.render("500.json", %{}) ==
             %{errors: %{detail: "Internal Server Error"}}
  end
end
