defmodule Wui6.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string, null: false
      add :created_at, :naive_datetime, null: false, default: fragment("CURRENT_TIMESTAMP")
      add :enabled, :boolean, null: false, default: true

      timestamps()
    end

    create unique_index(:users, [:email])
  end
end
