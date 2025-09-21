defmodule Wui6.Repo.Migrations.RemoveActivatedAtFromUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
    end
  end
end
