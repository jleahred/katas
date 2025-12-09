defmodule Wui7.Repo.Migrations.AddEnabledToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :enabled, :boolean, default: false, null: false
    end

    execute("UPDATE users SET enabled = 0 WHERE enabled IS NULL", fn -> :ok end)
  end
end
