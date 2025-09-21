defmodule Wui6.Roles do
  @moduledoc "Context for managing roles."

  import Ecto.Query, warn: false
  alias Wui6.Repo

  alias Wui6.Accounts.UserRole
  alias Wui6.Roles.Role

  @system_roles ~w(admin public)

  @doc "List all roles ordered by name."
  def list_roles(opts \\ []) do
    Role
    |> with_user_counts()
    |> maybe_limit(opts)
    |> Repo.all()
  end

  @doc "Fetch a role by id, returning nil when missing."
  def get_role(id), do: Repo.get(Role, id)

  @doc "Fetch a role by id, raising if not found."
  def get_role!(id), do: Repo.get!(Role, id)

  @doc "Create a role."
  def create_role(attrs \\ %{}) do
    %Role{}
    |> Role.changeset(attrs)
    |> Repo.insert()
  end

  @doc "Update a role."
  def update_role(%Role{} = role, attrs) do
    role
    |> Role.changeset(attrs)
    |> Repo.update()
  end

  @doc "Delete a role, returning an error when the role is in use."
  def delete_role(%Role{} = role) do
    Repo.delete(role)
  rescue
    _ in Ecto.ConstraintError -> {:error, :role_in_use}
  end

  @doc "Change role."
  def change_role(%Role{} = role, attrs \\ %{}) do
    Role.changeset(role, attrs)
  end

  @doc "Returns the list of roles managed como parte del sistema."
  def system_roles, do: @system_roles

  defp with_user_counts(query) do
    from r in query,
      left_join: ur in UserRole,
      on: ur.role_id == r.id,
      group_by: r.id,
      order_by: [asc: r.name],
      select_merge: %{users_count: count(ur.user_id)}
  end

  defp maybe_limit(query, opts) do
    case Keyword.get(opts, :limit) do
      nil -> query
      limit when is_integer(limit) and limit > 0 -> limit(query, ^limit)
      _ -> query
    end
  end
end
