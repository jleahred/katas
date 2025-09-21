defmodule Wui6.Repo.Migrations.CreateUserRoles do
  use Ecto.Migration

  def change do
    create table(:user_roles, primary_key: false) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :role_id, references(:roles, on_delete: :restrict), null: false

      timestamps(updated_at: false)
    end

    create unique_index(:user_roles, [:user_id, :role_id])
  end
end
