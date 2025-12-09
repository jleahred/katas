defmodule Wui7Web.ErrorJSONTest do
  use Wui7Web.ConnCase, async: true

  test "renders 404" do
    assert Wui7Web.ErrorJSON.render("404.json", %{}) == %{errors: %{detail: "Not Found"}}
  end

  test "renders 500" do
    assert Wui7Web.ErrorJSON.render("500.json", %{}) ==
             %{errors: %{detail: "Internal Server Error"}}
  end
end
