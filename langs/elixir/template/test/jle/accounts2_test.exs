defmodule Jle.Accounts2Test do
  use Jle.DataCase

  alias Jle.Accounts2

  describe "users2" do
    alias Jle.Accounts2.User2

    import Jle.Accounts2Fixtures

    @invalid_attrs %{age: nil, name: nil}

    test "list_users2/0 returns all users2" do
      user2 = user2_fixture()
      assert Accounts2.list_users2() == [user2]
    end

    test "get_user2!/1 returns the user2 with given id" do
      user2 = user2_fixture()
      assert Accounts2.get_user2!(user2.id) == user2
    end

    test "create_user2/1 with valid data creates a user2" do
      valid_attrs = %{age: 42, name: "some name"}

      assert {:ok, %User2{} = user2} = Accounts2.create_user2(valid_attrs)
      assert user2.age == 42
      assert user2.name == "some name"
    end

    test "create_user2/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Accounts2.create_user2(@invalid_attrs)
    end

    test "update_user2/2 with valid data updates the user2" do
      user2 = user2_fixture()
      update_attrs = %{age: 43, name: "some updated name"}

      assert {:ok, %User2{} = user2} = Accounts2.update_user2(user2, update_attrs)
      assert user2.age == 43
      assert user2.name == "some updated name"
    end

    test "update_user2/2 with invalid data returns error changeset" do
      user2 = user2_fixture()
      assert {:error, %Ecto.Changeset{}} = Accounts2.update_user2(user2, @invalid_attrs)
      assert user2 == Accounts2.get_user2!(user2.id)
    end

    test "delete_user2/1 deletes the user2" do
      user2 = user2_fixture()
      assert {:ok, %User2{}} = Accounts2.delete_user2(user2)
      assert_raise Ecto.NoResultsError, fn -> Accounts2.get_user2!(user2.id) end
    end

    test "change_user2/1 returns a user2 changeset" do
      user2 = user2_fixture()
      assert %Ecto.Changeset{} = Accounts2.change_user2(user2)
    end
  end
end
