defmodule PhoenixKatasWeb.FixLogControllerLV do
  use PhoenixKatasWeb, :controller
  alias PhoenixKatas.FixLogSch
  alias PhoenixKatas.RepoTrading
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
        records: records_db(form_params)
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
      "connection" => "",
      "date" => today_string(),
      "page" => "0",
      "msg_type" => "any",
      "exec_type" => "any"
    }
  end

  defp records_fake(_par) do
    [
      %PhoenixKatas.FixLogSch{
        account: "BTRNO",
        clordid: "6u1a1",
        connection: "cli_bbva",
        exectype: "4",
        fix:
          "8=FIX.4.3|9=279|35=8|34=1440|49=IMV|50=CIMD|52=20190416-15:55:00|56=BBV|57=bbvatrader19|1=BTRNO|6=0|11=6u1a1|14=0|15=EUR|17=105_175500_209|20=0|22=4|37=4782-10|38=4080|39=4|40=2|41=6u1a1|44=5.43|48=IT0003128367|54=1|55=ENEL|59=0|60=20190416-15:55:00.000|109=BTRNO|150=4|151=4080|167=CS|207=MTAA|10=199|\n",
        id: 41_005_257,
        msgtype: "ExecutionReport",
        origclordid: "6u1a1",
        price: "5.43",
        quantity: 4080,
        securityid: "IT0003128367",
        side: 1,
        symbol: "ENEL",
        time: "15:55:00.776",
        timeinforce: "0"
      },
      %PhoenixKatas.FixLogSch{
        account: "BTRNO",
        clordid: "4u1a1",
        connection: "cli_bbva",
        exectype: "4",
        fix:
          "8=FIX.4.3|9=281|35=8|34=1439|49=IMV|50=CIMD|52=20190416-15:55:00|56=BBV|57=bbvatrader19|1=BTRNO|6=0|11=4u1a1|14=0|15=EUR|17=105_175500_208|20=0|22=4|37=4782-1|38=10881|39=4|40=2|41=4u1a1|44=5.414|48=IT0003128367|54=1|55=ENEL|59=0|60=20190416-15:55:00.000|109=BTRNO|150=4|151=10881|167=CS|207=MTAA|10=049|\n",
        id: 41_005_256,
        msgtype: "ExecutionReport",
        origclordid: "4u1a1",
        price: "5.414",
        quantity: 10881,
        securityid: "IT0003128367",
        side: 1,
        symbol: "ENEL",
        time: "15:55:00.757",
        timeinforce: "0"
      },
      %PhoenixKatas.FixLogSch{
        account: "BTRNO",
        clordid: "8u1a1",
        connection: "cli_bbva",
        exectype: "4",
        fix:
          "8=FIX.4.3|9=279|35=8|34=1438|49=IMV|50=CIMD|52=20190416-15:55:00|56=BBV|57=bbvatrader19|1=BTRNO|6=0|11=8u1a1|14=0|15=EUR|17=105_175500_207|20=0|22=4|37=4782-9|38=4081|39=4|40=2|41=8u1a1|44=5.436|48=IT0003128367|54=1|55=ENEL|59=0|60=20190416-15:55:00.000|109=BTRNO|150=4|151=4081|167=CS|207=MTAA|10=224|\n",
        id: 41_005_255,
        msgtype: "ExecutionReport",
        origclordid: "8u1a1",
        price: "5.436",
        quantity: 4081,
        securityid: "IT0003128367",
        side: 1,
        symbol: "ENEL",
        time: "15:55:00.735",
        timeinforce: "0"
      }
    ]
    |> Stream.map(&FixLogSch.normalize_record(&1))
    |> Enum.into([])
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

    exec_type =
      if par["exec_type"] == "" or par["exec_type"] == "any" do
        "%"
      else
        "%" <> par["exec_type"]
      end

    query =
      from(fl in FixLogSch,
        # hints: ["USE INDEX (fix_log_7f12bbd9)"], # it doesn't work with our mariadb
        limit: 25,
        offset: ^(25 * String.to_integer(par["page"])),
        where:
          fl.time > ^par["date"] and fl.time <= ^(par["date"] <> " 23:59:59.999999") and
            like(fl.msgtype, ^msgtype) and
            like(fl.fix, ^any) and
            like(fl.connection, ^connection) and
            like(fl.exectype, ^exec_type),
        order_by: [desc: fl.time],
        select: fl
      )

    # RepoTrading.all(query)
    # |> Stream.map(&Map.update!(&1, :fix, fn el -> String.replace(el, <<1>>, "^") end))
    # |> Stream.map(&Map.update!(&1, :msgtype, fn mt -> Fix.Static.MsgTypes.get_name(mt) end))
    # |> Stream.map(&Map.update!(&1, :time, fn t -> String.slice(t, 11..50) end))

    RepoTrading.all(query)
    |> Stream.map(&FixLogSch.normalize_record(&1))
    |> Enum.into([])
  end
end
