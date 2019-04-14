defmodule PhoenixKatasWeb.FactorialView do
  use PhoenixKatasWeb, :view

  def render("factorial.html", par) do
    %{num: num, fact: fact} = par.conn.assigns
    "factorial of #{num} is #{fact}"
    # {num, _} = Integer.parse(par["num"])
  end
end
