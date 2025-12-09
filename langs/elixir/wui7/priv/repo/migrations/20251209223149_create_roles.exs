defmodule Wui7.Repo.Migrations.CreateRoles do
  use Ecto.Migration

  def change do
    create table(:roles) do
      add :code, :string, null: false
      add :description, :string, null: false

      timestamps(type: :utc_datetime)
    end

    create unique_index(:roles, [:code])
  end
end
