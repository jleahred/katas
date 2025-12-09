defmodule Wui7.Repo.Migrations.DowncaseRoleCodes do
  use Ecto.Migration

  def change do
    execute("UPDATE roles SET code = lower(trim(code))", fn -> :ok end)
  end
end
