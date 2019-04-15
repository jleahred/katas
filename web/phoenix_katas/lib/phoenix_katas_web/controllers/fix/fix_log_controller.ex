defmodule PhoenixKatasWeb.FixLogController do
  use PhoenixKatasWeb, :controller

  # ?date=2019-04-14&dir=Both&connection=Any&msg_type=Any&any=aa
  # %{"any" => "aa", "connection" => "Any", "date" => "2019-04-14", "dir" => "Both", "msg_type" => "Any"}
  @spec fix_log(Plug.Conn.t(), atom() | binary() | keyword() | map()) :: Plug.Conn.t()
  def fix_log(conn, par) do
    render(conn, "fix_log.html", form: Map.merge(def_par(), par))
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

  defp def_par() do
    %{
      "any" => "",
      "connection" => "any",
      "date" => today_string(),
      "dir" => "both",
      "msg_type" => "any"
    }
  end

  # defp par2atomkey(smap) do
  #   sdefpar =
  #     def_par()
  #     |> Enum.map(fn {k, v} ->
  #       {Atom.to_string(k), v}
  #     end)
  #     |> Enum.into(%{})

  #   smap
  #   |> Enum.filter(&Map.has_key?(sdefpar, elem(&1, 0)))
  #   |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
  #   |> Enum.into(%{})
  # end
end
