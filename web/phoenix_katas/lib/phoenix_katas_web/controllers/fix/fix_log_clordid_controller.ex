defmodule PhoenixKatasWeb.FixLogClOrdIdController do
  use PhoenixKatasWeb, :controller
  alias PhoenixKatas.FixLogSch
  alias PhoenixKatas.RepoTrading
  import Ecto.Query
  require Logger

  # ?clordid=J4HDA00CY0002&connection=dest_vt_mifid&date=2019-04-17
  # J4HDA00CY0002
  def show(conn, par) do
    # render(conn, "fix_log.html",
    #   form: form_params,
    #   records: records_db(form_params)
    # )

    # text(conn, "#{inspect(par)}")
    # records = records_db(par)

    # located = [par["clordid"]] |> Enum.into(MapSet.new())
    # looked = [] |> Enum.into(MapSet.new())
    # to_look = MapSet.difference(located, looked)
    # text(conn, "#{inspect(to_look)}")

    # text(conn, "#{inspect(get_rec_all_clordid(located, looked))}")

    # located = ["1904171441243377505", "J4HDA00CY0001", "J4HDA00CY0002"] |> Enum.into(MapSet.new())
    # looked = ["1904171441243377505", "J4HDA00CY0001"] |> Enum.into(MapSet.new())

    located = ["J4HDA00CY0001"] |> Enum.into(MapSet.new())
    looked = [] |> Enum.into(MapSet.new())
    records = get_rec_all_clordid(located, looked, par["date"], par["connection"])
    text(conn, "#{inspect(records)}")

    # records_clordids(get_rec_all_clordid(located, looked, par["date"], par["connection"]))
    # text(conn, "#{inspect(records)}")
  end

  defp get_rec_all_clordid(located, looked, date, connection) do
    pending2look = located |> MapSet.difference(looked)

    if MapSet.size(pending2look) == 0 do
      located
    else
      looked = looked |> Enum.into(pending2look)

      pending2look =
        pending2look
        |> Enum.reduce(located, fn nl, acc ->
          Logger.info("looking in db ... #{inspect(nl)}")
          Logger.info("found in db ... #{inspect(records_clordid(nl, date, connection))}")

          acc
          |> Enum.into(records_clordid(nl, date, connection))
        end)
        |> Enum.into(looked)

      Logger.info("pending2look 222 ... #{inspect(pending2look)}")

      Logger.info("looked2 ... #{inspect(looked)}")
      get_rec_all_clordid(pending2look, looked, date, connection)
    end
  end

  defp records_clordid(clordid, date, connection) do
    query =
      from(fl in FixLogSch,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        # limit: 30,
        where:
          fl.time > ^date and fl.time <= ^"#{date} 23:59:59.999999" and
            (like(fl.clordid, ^clordid) or like(fl.origclordid, ^clordid)) and
            like(fl.connection, ^connection),
        order_by: [desc: fl.time],
        select: %{clordid: fl.clordid, origclordid: fl.origclordid}
      )

    RepoTrading.all(query)
    |> Enum.reduce([], fn %{clordid: clordid, origclordid: origclordid}, acc ->
      [clordid | [origclordid | acc]]
    end)
    |> Enum.filter(&(&1 != ""))
    |> Enum.into(MapSet.new())
  end

  defp records_clordids(clordids, date, connection) do
    query =
      from(fl in FixLogSch,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        # limit: 30,
        where:
          fl.time > ^date and fl.time <= ^"#{date} 23:59:59.999999" and
            (fl.clordid in ^clordids or fl.origclordid in ^clordids) and
            like(fl.connection, ^connection),
        order_by: [desc: fl.time],
        select: %{clordid: fl.clordid, origclordid: fl.origclordid}
      )

    RepoTrading.all(query)
    |> Enum.reduce([], fn %{clordid: clordid, origclordid: origclordid}, acc ->
      [clordid | [origclordid | acc]]
    end)
    |> Enum.filter(&(&1 != ""))
    |> Enum.into(MapSet.new())
  end
end
