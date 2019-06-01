defmodule JLEWeb.CwsStatsDelayController do
  use JLEWeb, :controller
  # alias JLE.AlarmsSch
  alias JLE.RepoAlarms
  # import Ecto.Query
  require Logger

  def index(conn, %{}) do
    # text(
    #   conn,
    #   "#{inspect(get_delays_today() |> Enum.map(&(&1 |> rec2del_list)), pretty: true)}"
    # )

    render(conn, "index.html", records: get_delays_today() |> Enum.map(&(&1 |> rec2del_list)))

    # generate_file(get_delays_from(101_172_355).rows)

    # text(conn, "#{inspect(get_delays_from(101_172_355).rows, pretty: true)}")
    # text(conn, "Finished")
  end

  # defp generate_file(rows) do
  #   {:ok, file} = File.open("./local_files/cws_delays.txt", [:write])

  #   rows
  #   |> Enum.map(&rec2csv(&1))
  #   # |> Logger.info()
  #   |> Enum.map(&IO.binwrite(file, &1))

  #   File.close(file)
  # end

  # defp get_delays_from(from) do
  #   query = """
  #       -- explain
  #       -- 101172355

  #       select id_cli_alarms, db_time, machine, description, process_version, process_uuid

  #       from CLI_ALARMS
  #       FORCE index(IDX_CLI_ALARMS_DBTSUBJ)

  #       where DB_TIME > DATE_ADD(current_date(), INTERVAL -30 DAY) AND DB_TIME < DATE_ADD(current_date(), INTERVAL -14 DAY)
  #       -- where id_cli_alarms > #{from}

  #       and subject like 'fluct%'

  #       and machine not like 'dub@%'

  #       and subject like 'fluct.local_pp%'

  #       and extract(hour from db_time) > 9
  #       and extract(hour from db_time) < 18

  #       -- and description like '5min%'
  #       and description like 'prev%'

  #       and machine not like 'uni%'

  #       -- limit 100
  #   """

  #   Ecto.Adapters.SQL.query!(RepoAlarms, query, [])
  # end

  defp get_delays_today() do
    query = """
        -- explain

        select id_cli_alarms, db_time, machine, description, process_version, process_uuid

        from CLI_ALARMS
        FORCE index(IDX_CLI_ALARMS_DBTSUBJ)

        where DB_TIME > DATE_ADD(current_date(), INTERVAL -0 DAY) AND DB_TIME < DATE_ADD(current_date(), INTERVAL +1 DAY)

        and subject like 'fluct%'
        and machine not like 'dub@%'
        and subject like 'fluct.local_pp%'
        and extract(hour from db_time) > 8
        and extract(hour from db_time) < 18
        -- and description like '5min%'
        and description like 'prev%'
        and machine not like 'uni%'
        and machine like 'ainhoa%'

        -- limit 10
    """

    Ecto.Adapters.SQL.query!(RepoAlarms, query, []).rows
  end

  defp get_delays_today_fake() do
    [
      [
        110_227_923,
        ~N[2019-05-10 10:00:01],
        "ainhoa@IMV107",
        "prev:  250ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_000,
        ~N[2019-05-12 10:00:08],
        "ainhoa@IMV107",
        "prev:  265ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_002,
        ~N[2019-05-17 10:00:09],
        "ainhoa@IMV107",
        "prev: -  218ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_005,
        ~N[2019-05-21 10:00:10],
        "ainhoa@IMV107",
        "prev:  202ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_027,
        ~N[2019-05-22 10:00:13],
        "ainhoa@IMV107",
        "prev: -  280ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_043,
        ~N[2019-05-24 10:00:14],
        "ainhoa@IMV107",
        "prev:  1s  778ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_075,
        ~N[2019-05-26 10:00:17],
        "ainhoa@IMV107",
        "prev: -  1s  108ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_087,
        ~N[2019-05-27 10:00:17],
        "ainhoa@IMV107",
        "prev: -  498ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_092,
        ~N[2019-05-27 10:00:18],
        "ainhoa@IMV107",
        "prev:  498ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ],
      [
        110_228_150,
        ~N[2019-05-29 10:00:20],
        "ainhoa@IMV107",
        "prev:  1s  186ms context last check ......",
        "2.56.0.140",
        "8514E104"
      ]
    ]
  end

  defp extract_delay(orig_delay) do
    c =
      Regex.named_captures(
        ~r/prev: -? *((?<d>(([0-9])+))d)? *((?<h>(([0-9])+))h)? *((?<m>(([0-9])+))m)? *((?<s>(([0-9])+))s)? *((?<ms>(([0-9])+))ms)?( |$).*/,
        orig_delay
      )

    (c["h"] |> to_integer_or_zero()) * 60 * 60 * 1000 +
      (c["m"] |> to_integer_or_zero()) * 60 * 1000 +
      (c["s"] |> to_integer_or_zero()) * 1000 +
      (c["ms"] |> to_integer_or_zero())
  end

  defp to_integer_or_zero(s) do
    case s do
      "" -> 0
      other -> String.to_integer(other)
    end
  end

  defp rec2del_list([_id, date, user, delay, _ver, _proc_id]) do
    [date, user, delay |> extract_delay()]
  end

  # defp rec2csv(r) do
  #   Logger.info(inspect(r))

  #   {[], result} =
  #     {r, ""}
  #     |> write_insp
  #     |> write_date_time
  #     |> write_field
  #     |> write_delay
  #     |> write_field
  #     |> write_field

  #   result <> "\n"
  # end

  # defp write_field({[h | t], result}) do
  #   result = result <> h <> ","
  #   {t, result}
  # end

  # defp write_insp({[h | t], result}) do
  #   result = result <> inspect(h) <> ","
  #   {t, result}
  # end

  # defp write_date_time({[h | t], result}) do
  #   result = result <> (h |> NaiveDateTime.to_string()) <> ","
  #   {t, result}
  # end

  # defp write_delay({[h | t], result}) do
  #   Logger.info(inspect(h))

  #   c =
  #     Regex.named_captures(
  #       ~r/prev: -? *((?<d>(([0-9])+))d)? *((?<h>(([0-9])+))h)? *((?<m>(([0-9])+))m)? *((?<s>(([0-9])+))s)? *((?<ms>(([0-9])+))ms)? .*/,
  #       h
  #     )

  #   d =
  #     "#{String.pad_leading(c["h"], 2, "0")}:#{String.pad_leading(c["m"], 2, "0")}:#{
  #       String.pad_leading(c["s"], 2, "0")
  #     }.#{String.pad_leading(c["ms"], 3, "0")}"

  #   Logger.info(inspect(c))
  #   Logger.info(inspect(d))
  #   result = result <> d <> ","
  #   {t, result}
  # end
end
