defmodule Wui6.Repo.Migrations.RemoveCreatedAtFromUsers do
  use Ecto.Migration

  def up do
    alter table(:users) do
      remove :created_at
    end
  end

  def down do
    alter table(:users) do
      add :created_at, :naive_datetime, null: false, default: fragment("CURRENT_TIMESTAMP")
    end
  end
end
