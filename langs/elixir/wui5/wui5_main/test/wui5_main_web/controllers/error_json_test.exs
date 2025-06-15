defmodule Wui5MainWeb.ErrorJSONTest do
  use Wui5MainWeb.ConnCase, async: true

  test "renders 404" do
    assert Wui5MainWeb.ErrorJSON.render("404.json", %{}) == %{errors: %{detail: "Not Found"}}
  end

  test "renders 500" do
    assert Wui5MainWeb.ErrorJSON.render("500.json", %{}) ==
             %{errors: %{detail: "Internal Server Error"}}
  end
end
