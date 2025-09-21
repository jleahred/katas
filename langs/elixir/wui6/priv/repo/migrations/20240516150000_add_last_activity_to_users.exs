defmodule Wui6.Repo.Migrations.AddLastActivityToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :last_activity, :naive_datetime
    end

    create index(:users, [:last_activity])
  end
end
