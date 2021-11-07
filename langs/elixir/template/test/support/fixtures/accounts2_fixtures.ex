defmodule Jle.Accounts2Fixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Jle.Accounts2` context.
  """

  @doc """
  Generate a user2.
  """
  def user2_fixture(attrs \\ %{}) do
    {:ok, user2} =
      attrs
      |> Enum.into(%{
        age: 42,
        name: "some name"
      })
      |> Jle.Accounts2.create_user2()

    user2
  end
end
