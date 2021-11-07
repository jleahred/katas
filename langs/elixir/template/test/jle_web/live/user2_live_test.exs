defmodule JleWeb.User2LiveTest do
  use JleWeb.ConnCase

  import Phoenix.LiveViewTest
  import Jle.Accounts2Fixtures

  @create_attrs %{age: 42, name: "some name"}
  @update_attrs %{age: 43, name: "some updated name"}
  @invalid_attrs %{age: nil, name: nil}

  defp create_user2(_) do
    user2 = user2_fixture()
    %{user2: user2}
  end

  describe "Index" do
    setup [:create_user2]

    test "lists all users2", %{conn: conn, user2: user2} do
      {:ok, _index_live, html} = live(conn, Routes.user2_index_path(conn, :index))

      assert html =~ "Listing Users2"
      assert html =~ user2.name
    end

    test "saves new user2", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, Routes.user2_index_path(conn, :index))

      assert index_live |> element("a", "New User2") |> render_click() =~
               "New User2"

      assert_patch(index_live, Routes.user2_index_path(conn, :new))

      assert index_live
             |> form("#user2-form", user2: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#user2-form", user2: @create_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.user2_index_path(conn, :index))

      assert html =~ "User2 created successfully"
      assert html =~ "some name"
    end

    test "updates user2 in listing", %{conn: conn, user2: user2} do
      {:ok, index_live, _html} = live(conn, Routes.user2_index_path(conn, :index))

      assert index_live |> element("#user2-#{user2.id} a", "Edit") |> render_click() =~
               "Edit User2"

      assert_patch(index_live, Routes.user2_index_path(conn, :edit, user2))

      assert index_live
             |> form("#user2-form", user2: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#user2-form", user2: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.user2_index_path(conn, :index))

      assert html =~ "User2 updated successfully"
      assert html =~ "some updated name"
    end

    test "deletes user2 in listing", %{conn: conn, user2: user2} do
      {:ok, index_live, _html} = live(conn, Routes.user2_index_path(conn, :index))

      assert index_live |> element("#user2-#{user2.id} a", "Delete") |> render_click()
      refute has_element?(index_live, "#user2-#{user2.id}")
    end
  end

  describe "Show" do
    setup [:create_user2]

    test "displays user2", %{conn: conn, user2: user2} do
      {:ok, _show_live, html} = live(conn, Routes.user2_show_path(conn, :show, user2))

      assert html =~ "Show User2"
      assert html =~ user2.name
    end

    test "updates user2 within modal", %{conn: conn, user2: user2} do
      {:ok, show_live, _html} = live(conn, Routes.user2_show_path(conn, :show, user2))

      assert show_live |> element("a", "Edit") |> render_click() =~
               "Edit User2"

      assert_patch(show_live, Routes.user2_show_path(conn, :edit, user2))

      assert show_live
             |> form("#user2-form", user2: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        show_live
        |> form("#user2-form", user2: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.user2_show_path(conn, :show, user2))

      assert html =~ "User2 updated successfully"
      assert html =~ "some updated name"
    end
  end
end
