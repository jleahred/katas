defmodule PhoenixKatasWeb.FixLogController do
  use PhoenixKatasWeb, :controller

  # ?date=2019-04-14&dir=Both&connection=Any&msg_type=Any&any=aa
  # %{"any" => "aa", "connection" => "Any", "date" => "2019-04-14", "dir" => "Both", "msg_type" => "Any"}
  @spec fix_log(Plug.Conn.t(), atom() | binary() | keyword() | map()) :: Plug.Conn.t()
  def fix_log(conn, par) do
    render(conn, "fix_log.html", par)
    # text(conn, "#{inspect(par)}")
    # text(conn, "here")
  end
end
