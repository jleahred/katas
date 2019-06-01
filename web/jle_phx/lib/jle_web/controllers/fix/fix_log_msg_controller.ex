defmodule JLEWeb.FixLogMsgController do
  use JLEWeb, :controller
  alias JLE.RepoTrading
  alias JLE.FixLogSch

  def show(conn, %{"idmsg" => "0"}) do
    render(conn, "fix_log_msg.html", fix_msg: parse_fix_msg(fake_msg()))
    # text(conn, "#{inspect(fake_msg())}")
  end

  # %{"idmsg" => "0"}
  def show(conn, %{"idmsg" => id}) do
    render(conn, "fix_log_msg.html", fix_msg: parse_fix_msg(record_db(id).fix))
    # text(conn, "pending...#{inspect(id)}")
    # text(conn, "pending...#{inspect(parse_fix_msg(record_db(id).fix))}")
  end

  def fake_msg() do
    "8=FIX.4.4|9=126|35=A|49=theBroker.12345|56=CSERVER|34=1|52=20170117- 08:03:04|57=TRADE|50=any_string|98=0|108=30|141=Y|553=12345|554=passw0rd!|10=131|"
  end

  def parse_fix_msg(fix_msg) do
    fix_msg
    |> String.split("|")
    |> Stream.filter(&(&1 != "\n"))
    |> Stream.map(&String.split(&1, "="))
    |> Stream.filter(&(&1 != [""]))
    |> Stream.map(fn [tag, val] -> {tag, Fix.Static.Tags.get_name(tag), val} end)
    |> Enum.into([])
  end

  defp record_db(id) do
    RepoTrading.get(FixLogSch, id)
    |> FixLogSch.normalize_record()
  end
end
