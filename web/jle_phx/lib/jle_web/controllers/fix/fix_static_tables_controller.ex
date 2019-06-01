defmodule JLEWeb.FixStaticTableController do
  use JLEWeb, :controller

  def index(conn, %{"type" => "currencies"}) do
    render(
      conn,
      "index.html",
      fn_all_codes: &Fix.Static.Currencies.list_all_codes/0,
      fn_get_name: &Fix.Static.Currencies.get_name/1
    )
  end

  def index(conn, %{"type" => "msg_types"}) do
    render(
      conn,
      "index.html",
      fn_all_codes: &Fix.Static.MsgTypes.list_all_codes/0,
      fn_get_name: &Fix.Static.MsgTypes.get_name/1
    )
  end

  def index(conn, %{"type" => "exec_types"}) do
    render(
      conn,
      "index.html",
      fn_all_codes: &Fix.Static.ExecTypes.list_all_codes/0,
      fn_get_name: &Fix.Static.ExecTypes.get_name/1
    )
  end

  def index(conn, %{"type" => "order_types"}) do
    render(
      conn,
      "index.html",
      fn_all_codes: &Fix.Static.OrderTypes.list_all_codes/0,
      fn_get_name: &Fix.Static.OrderTypes.get_name/1
    )
  end

  def index(conn, %{"type" => "peg_price_type"}) do
    render(
      conn,
      "index.html",
      fn_all_codes: &Fix.Static.OrderTypes.list_all_codes/0,
      fn_get_name: &Fix.Static.OrderTypes.get_name/1
    )
  end

  # text(conn, "#{inspect(par)}")
  # text(conn, "here")
end
