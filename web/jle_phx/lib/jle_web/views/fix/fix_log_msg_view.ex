defmodule JLEWeb.FixLogMsgView do
  use JLEWeb, :view

  # def render("fix_log.html", par) do
  #   "#{inspect(par)}"
  # end

  def val_riched(tag, val) do
    case tag do
      "35" -> "#{val} #(#{Fix.Static.MsgTypes.get_name(val)})"
      "150" -> "#{val} #(#{Fix.Static.ExecTypes.get_name(val)})"
      "59" -> "#{val} #(#{Fix.Static.TimeInForce.get_name(val)})"
      "54" -> "#{val} #(#{side_text(val)})"
      "15" -> "#{val} #(#{Fix.Static.Currencies.get_name(val)})"
      "40" -> "#{val} #(#{Fix.Static.OrderTypes.get_name(val)})"
      "1094" -> "#{val} #(#{Fix.Static.PegPriceType.get_name(val)})"
      _ -> "#{val}"
    end
  end

  def side_text(s) do
    case s do
      "1" -> "Buy"
      "2" -> "Sell"
      _ -> "?"
    end
  end
end
