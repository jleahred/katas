defmodule JLEWeb.FixLogClOrdIdController do
  use JLEWeb, :controller
  alias JLE.FixLogSch
  alias JLE.RepoTrading
  import Ecto.Query
  alias Phoenix.LiveView
  require Logger

  # ?clordid=19042998543529854352FA00JH0002&connection=dest_sibe_eq_bmex&date=2019-04-29
  def show(conn, par) do
    # render(conn, "fix_log.html",
    #   form: form_params,
    #   records: records_db(form_params)
    # )

    located = [par["clordid"]] |> Enum.into(MapSet.new())
    looked = [] |> Enum.into(MapSet.new())
    cl_ord_ids = get_rec_all_clordid(located, looked, par["date"], par["connection"])
    records = records_db(par["date"], par["connection"], cl_ord_ids |> Enum.into([]))
    # text(conn, "#{inspect(records, pretty: true)}")

    LiveView.Controller.live_render(conn, JLEWeb.FixLogLive,
      session: %{
        params: %{},
        date: par["date"],
        records: records,
        show_filter_form: false,
        clordid_link: false
      }
    )
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
          acc
          |> Enum.into(records_clordid(nl, date, connection))
        end)
        |> Enum.into(looked)

      get_rec_all_clordid(pending2look, looked, date, connection)
    end
  end

  defp records_clordid(clordid, date, connection) do
    query =
      from(fl in FixLogSch,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        limit: 30,
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

  defp records_db(date, connection, cl_ord_ids) do
    query =
      from(fl in FixLogSch,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        limit: 25,
        # offset: ^(25 * String.to_integer(par["page"])),
        where:
          fl.time > ^date and fl.time <= ^(date <> " 23:59:59.999999") and
            (fl.clordid in ^cl_ord_ids or fl.origclordid in ^cl_ord_ids) and
            like(fl.connection, ^connection),
        order_by: [desc: fl.time],
        select: fl
      )

    RepoTrading.all(query)
    |> Stream.map(&FixLogSch.normalize_record(&1))
    |> Enum.into([])
  end
end
