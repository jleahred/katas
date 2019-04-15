defmodule PhoenixKatasWeb.FixLogController do
  use PhoenixKatasWeb, :controller
  alias PhoenixKatas.FixLogSch
  alias PhoenixKatas.Repo
  import Ecto.Query

  # ?date=2019-04-14&dir=Both&connection=Any&msg_type=Any&any=aa
  # %{"any" => "aa", "connection" => "Any", "date" => "2019-04-14", "dir" => "Both", "msg_type" => "Any"}
  @spec fix_log(Plug.Conn.t(), atom() | binary() | keyword() | map()) :: Plug.Conn.t()
  def fix_log(conn, par) do
    form_params = Map.merge(default_par(), par)

    render(conn, "fix_log.html", form: form_params, records: records(form_params))
    # text(conn, "#{inspect(records(form_params))}")

    # text(conn, "#{inspect(Map.merge(def_par(), par2atomkey(par)))}")
    # text(conn, "#{inspect(par2atomkey(par))}")
    # text(conn, "#{inspect(def_par())}")
    # render(conn, "fix_log.html", date: today_string())
    # text(conn, "#{d.year}-#{d.month}-#{d.day}")
    # text(conn, "#{inspect(par)}")
    # text(conn, "#{inspect(string_map2atom_map(par))}")
    # text(conn, "here")
  end

  @spec today_string() :: <<_::16, _::_*8>>
  defp today_string() do
    d = DateTime.utc_now()
    year = d.year |> Integer.to_string() |> String.pad_leading(4, "0")
    month = d.month |> Integer.to_string() |> String.pad_leading(2, "0")
    day = d.day |> Integer.to_string() |> String.pad_leading(2, "0")
    "#{year}-#{month}-#{day}"
  end

  defp default_par() do
    %{
      "any" => "",
      "connection" => "any",
      "date" => today_string(),
      "dir" => "both",
      "msg_type" => "any"
    }
  end

  defp records(par) do
    # fake testing
    # [
    #   %PhoenixKatas.FixLogSch{
    #     account: "",
    #     clordid: "",
    #     connection: "dest_bimi",
    #     exectype: "",
    #     fix:
    #       "8=FIX.4.2^9=61^35=A^34=1^49=IMV^52=20170717-05:45:05^56=MKTHUBP^98=0^108=20^10=063^\n",
    #     id: 26_919_976,
    #     msgtype: "A",
    #     origclordid: "",
    #     securityid: "",
    #     side: 0,
    #     symbol: "",
    #     time: "2017-07-17 05:45:05.276"
    #   },
    #   %PhoenixKatas.FixLogSch{
    #     account: "",
    #     clordid: "",
    #     connection: "dest_bimi",
    #     exectype: "",
    #     fix:
    #       "8=FIX.4.2^9=65^35=A^34=1^49=MKTHUBP^52=20170717-05:45:05.285^56=IMV^98=0^108=20^10=016^\n",
    #     id: 26_919_977,
    #     msgtype: "A",
    #     origclordid: "",
    #     securityid: "",
    #     side: 0,
    #     symbol: "",
    #     time: "2017-07-17 05:45:05.303"
    #   },
    #   %PhoenixKatas.FixLogSch{
    #     account: "",
    #     clordid: "",
    #     connection: "conf_bankia",
    #     exectype: "",
    #     fix: "8=FIX.4.1^9=57^35=A^34=1^49=CAM^56=IMV^52=20170717-07:45:05^98=0^108=60^10=255^\n",
    #     id: 26_919_986,
    #     msgtype: "A",
    #     origclordid: "",
    #     securityid: "",
    #     side: 0,
    #     symbol: "",
    #     time: "2017-07-17 05:45:05.611"
    #   }
    # ]
    # par example %{"any" => "abc", "connection" => "bloomb", "date" => "2019-04-17", "dir" => "out", "msg_type" => "bbva"}

    any =
      if par["any"] == "" do
        "%"
      else
        "%" <> par["any"] <> "%"
      end

    msgtype =
      if par["msg_type"] == "" or par["msg_type"] == "any" do
        "%"
      else
        "%" <> par["msg_type"]
      end

    connection =
      if par["connection"] == "" or par["connection"] == "any" do
        "%"
      else
        "%" <> par["connection"]
      end

    # dir =
    #   case par["dir"] do
    #     "" -> [1, 2]
    #     "both" -> [1, 2]
    #     "in" -> [1]
    #     "out" -> [2]
    #   end

    query =
      from(fl in FixLogSch,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        limit: 30,
        # offset: 10,
        where:
          fl.time > ^par["date"] and fl.time <= ^(par["date"] <> " 23:59:59.999999") and
            like(fl.msgtype, ^msgtype) and
            like(fl.fix, ^any) and
            like(fl.connection, ^connection),
        # fl.dir in ^dir,
        order_by: [desc: fl.time],
        select: fl
      )

    Repo.all(query)
    |> Stream.map(&Map.update!(&1, :fix, fn el -> String.replace(el, <<1>>, "^") end))
    |> Stream.map(&Map.update!(&1, :msgtype, fn mt -> Fix.Static.MsgTypes.get_name(mt) end))
    |> Stream.map(&Map.update!(&1, :time, fn t -> String.slice(t, 11..50) end))
    |> Stream.map(
      &Map.update!(&1, :dir, fn d ->
        case d do
          _ -> "?"
        end
      end)
    )
    |> Enum.into([])

    # msgtype
  end
end
