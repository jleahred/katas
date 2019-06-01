defmodule JLEWeb.FixBeautifierController do
  use JLEWeb, :controller

  def index(conn, par) do
    separator = Map.get(par, "separator", "|")

    render(
      conn,
      "index.html",
      fix_msg: Map.get(par, "fix_msg", ""),
      fix_parsed: Map.get(par, "fix_msg", "") |> parse_fix_msg(separator),
      separator: separator
    )
  end

  defp parse_fix_msg(fix_msg, separator) do
    try do
      fix_msg
      |> String.split(separator)
      |> Stream.filter(&(&1 != "\n"))
      |> Stream.map(&String.split(&1, "="))
      |> Stream.filter(&(&1 != [""]))
      |> Stream.map(fn [tag, val] -> {tag, Fix.Static.Tags.get_name(tag), val} end)
      |> Enum.into([])
    rescue
      _ -> nil
    end
  end

  # text(conn, "#{inspect(par)}")
  # text(conn, "here")
end
