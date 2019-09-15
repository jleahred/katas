defmodule Solution do
  def main() do
    r = io_read_number()

    if r > 1 do
      1..r
      |> Enum.reduce([], fn _, acc -> [io_read_number() | acc] end)
      |> Enum.reverse()
      |> Enum.each(fn max ->
        (sum_mult(max, 3) + sum_mult(max, 5) - sum_mult(max, 15)) |> IO.puts()
      end)
    else
      IO.puts("")
    end
  end

  def sum_mult(max, mult) do
    n = div(max - 1, mult)
    div(n * (n + 1), 2) * mult
  end

  defp io_read_number() do
    IO.gets("") |> String.trim() |> String.to_integer()
  end
end

# Solution.main()
