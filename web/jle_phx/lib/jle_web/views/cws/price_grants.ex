defmodule JLEWeb.PriceGrantsView do
  use JLEWeb, :view

  # def render("index.html", par) do
  #   "#{inspect(par.conn.assigns.rep_info)}"
  # end

  defp filter_market_cup(market, cup) do
    cup
    |> Enum.filter(&(&1.market == market))
    |> Enum.filter(&(&1.user != "JOSELUIS.ESTEBAN@DESARROLLO"))
  end

  def hightlight_row(cup) do
    cond do
      cup.inactivity_days > 60 -> "bg-danger text-white"
      cup.user == "DANIEL.FERNANDEZ@RF" -> "bg-danger text-white"
      true -> ""
    end
  end
end
