defmodule PhoenixKatasWeb.CwsVersionsController do
  use PhoenixKatasWeb, :controller
  alias PhoenixKatas.AlarmsSch
  alias PhoenixKatas.RepoAlarms
  import Ecto.Query
  require Logger

  def index(conn, %{}) do
    render(conn, "index.html", records: get_versions_today())

    # text(conn, "#{inspect(get_versions_today(), pretty: true)}")
  end

  defp get_versions_today() do
    query =
      from(al in AlarmsSch.Cli,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        # limit: 3,
        group_by: [:broker_code, :machine, :process_name, :process_version],
        where: al.db_time > ^NaiveDateTime.utc_now(),
        # and al.process_name == "cent2",
        select: %{
          broker_code: al.broker_code,
          process_name: al.process_name,
          machine: al.machine,
          process_version: al.process_version
        }
      )

    RepoAlarms.all(query)
    |> Enum.map(fn r -> Map.put(r, :v, decompose_ver(r.process_version)) end)
    |> Enum.sort(fn r1, r2 ->
      {r1.broker_code, r1.process_name, r1.v} < {r2.broker_code, r2.process_name, r2.v}
    end)
  end

  defp decompose_ver(pv) do
    pv |> String.split(".")
  end
end
