defmodule Wui6.Repo.Migrations.CreateRoles do
  use Ecto.Migration

  def change do
    create table(:roles) do
      add :name, :string, null: false
      add :description, :text

      timestamps(type: :naive_datetime)
    end

    create unique_index(:roles, [:name])
  end
end
