defmodule JLEWeb.CwsStatsDelayView do
  use JLEWeb, :view

  # def render("fix_log.html", par) do
  #   "#{inspect(par)}"
  # end

  defp js_date_time(d) do
    "new Date( Date.UTC(#{d.year}, #{d.month - 1}, #{d.day}, #{d.hour}, #{d.minute}, #{d.second}) )"
  end
end
