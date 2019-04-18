defmodule PhoenixKatasWeb.FixLogControllerLV do
  use PhoenixKatasWeb, :controller
  alias PhoenixKatas.FixLogSch
  alias PhoenixKatas.Repo
  import Ecto.Query
  alias Phoenix.LiveView
  require Logger

  # ?date=2019-04-14&dir=Both&connection=Any&msg_type=Any&any=aa
  # %{"any" => "aa", "connection" => "Any", "date" => "2019-04-14", "dir" => "Both", "msg_type" => "Any"}
  def show(conn, par) do
    form_params = Map.merge(default_par(), par)

    LiveView.Controller.live_render(conn, PhoenixKatasWeb.FixLogLive,
      session: %{
        params: form_params,
        records:
          records_fake(form_params)
          |> Stream.map(&normalize_record(&1))
          |> Enum.into([])
      }
    )

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

  defp records_fake(par) do
    [
      %PhoenixKatas.FixLogSch{
        account: "",
        clordid: "",
        connection: "dest_bimi",
        exectype: "",
        fix:
          "8=FIX.4.2|9=61|35=A|34=1|49=IMV|52=20170717-05:45:05|56=MKTHUBP|98=0|108=20|10=063|\n",
        id: 26_919_976,
        msgtype: "A",
        origclordid: "",
        securityid: "",
        side: 0,
        symbol: "",
        time: "2017-07-17 05:45:05.276"
      },
      %PhoenixKatas.FixLogSch{
        account: "",
        clordid: "",
        connection: "dest_bimi",
        exectype: "",
        fix:
          "8=FIX.4.2|9=65|35=A|34=1|49=MKTHUBP|52=20170717-05:45:05.285|56=IMV|98=0|108=20|10=016|\n",
        id: 26_919_977,
        msgtype: "A",
        origclordid: "",
        securityid: "",
        side: 0,
        symbol: "",
        time: "2017-07-17 05:45:05.303"
      },
      %PhoenixKatas.FixLogSch{
        account: "",
        clordid: "",
        connection: "conf_bankia",
        exectype: "",
        fix: "8=FIX.4.1|9=57|35=A|34=1|49=CAM|56=IMV|52=20170717-07:45:05|98=0|108=60|10=255|\n",
        id: 26_919_986,
        msgtype: "A",
        origclordid: "",
        securityid: "",
        side: 0,
        symbol: "",
        time: "2017-07-17 05:45:05.611"
      }
    ]
  end

  # par example %{"any" => "abc", "connection" => "bloomb", "date" => "2019-04-17", "dir" => "out", "msg_type" => "bbva"}
  defp records_db(par) do
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

    # Repo.all(query)
    # |> Stream.map(&Map.update!(&1, :fix, fn el -> String.replace(el, <<1>>, "^") end))
    # |> Stream.map(&Map.update!(&1, :msgtype, fn mt -> Fix.Static.MsgTypes.get_name(mt) end))
    # |> Stream.map(&Map.update!(&1, :time, fn t -> String.slice(t, 11..50) end))

    Repo.all(query)
    # |> Stream.map(&normalize_record(&1))
    # |> Enum.into([])
  end

  def normalize_record(fix_msg) do
    fix_msg
    |> Map.update!(:fix, &String.replace(&1, <<01>>, "|"))
    |> Map.update!(:msgtype, &Fix.Static.MsgTypes.get_name(&1))
    |> Map.update!(:time, &String.slice(&1, 11..50))
  end
end
