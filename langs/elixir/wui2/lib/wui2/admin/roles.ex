# ------------------------------------------------------------------------
# ATTENTION compile time indexing...  :-O
roles = [
  %{id: 1, name: "admin", description: "full access"},
  %{id: 2, name: "admin_users", description: "create, delete, modif user information"},
  %{
    id: 3,
    name: "xxx_admin",
    description: "manage xxx grants and show, delete and create..."
  },
  %{
    id: 4,
    name: "xxx_user",
    description: "show, delete and create xxx for granted clients"
  }
]

roles_indexed_gen = fn ->
  roles
  |> Enum.reduce(%{}, fn r, acc ->
    acc |> Map.put(r.id, r)
  end)
end

# role_names = fn ->
#   nil
# end

# ATTENTION compile time indexing...  :-O
# ------------------------------------------------------------------------
#

defmodule Wui2.Roles do
  @roles_indexed roles_indexed_gen.()
  @roles roles

  def all() do
    @roles
  end

  def get_role(role_id) do
    @roles_indexed[role_id]
  end
end
