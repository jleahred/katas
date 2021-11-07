defmodule Jle.Accounts2 do
  @moduledoc """
  The Accounts2 context.
  """

  import Ecto.Query, warn: false
  alias Jle.Repo

  alias Jle.Accounts2.User2

  @doc """
  Returns the list of users2.

  ## Examples

      iex> list_users2()
      [%User2{}, ...]

  """
  def list_users2 do
    Repo.all(User2)
  end

  @doc """
  Gets a single user2.

  Raises `Ecto.NoResultsError` if the User2 does not exist.

  ## Examples

      iex> get_user2!(123)
      %User2{}

      iex> get_user2!(456)
      ** (Ecto.NoResultsError)

  """
  def get_user2!(id), do: Repo.get!(User2, id)

  @doc """
  Creates a user2.

  ## Examples

      iex> create_user2(%{field: value})
      {:ok, %User2{}}

      iex> create_user2(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_user2(attrs \\ %{}) do
    %User2{}
    |> User2.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a user2.

  ## Examples

      iex> update_user2(user2, %{field: new_value})
      {:ok, %User2{}}

      iex> update_user2(user2, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_user2(%User2{} = user2, attrs) do
    user2
    |> User2.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a user2.

  ## Examples

      iex> delete_user2(user2)
      {:ok, %User2{}}

      iex> delete_user2(user2)
      {:error, %Ecto.Changeset{}}

  """
  def delete_user2(%User2{} = user2) do
    Repo.delete(user2)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking user2 changes.

  ## Examples

      iex> change_user2(user2)
      %Ecto.Changeset{data: %User2{}}

  """
  def change_user2(%User2{} = user2, attrs \\ %{}) do
    User2.changeset(user2, attrs)
  end
end
