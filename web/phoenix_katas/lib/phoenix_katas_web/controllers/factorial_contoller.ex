defmodule PhoenixKatasWeb.FactorialController do
  use PhoenixKatasWeb, :controller

  # def factorial(conn, %{"num" => num}) do
  #   # render(conn, "search.html")
  #   text(conn, "OK")
  # end
  def factorial(conn, par) do
    {num, _} = Integer.parse(par["num"])

    render(conn, "factorial.html", num: num, fact: factorial(num))

    # text(conn, "#{inspect(par)}")
  end

  defp factorial(num) do
    1..num |> Enum.reduce(&(&1 * &2))
  end
end
