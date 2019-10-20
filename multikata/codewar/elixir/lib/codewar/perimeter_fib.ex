defmodule Perim do
  def perimeter(n) do
    (Stream.iterate({1, 1}, fn {a, b} -> {b, a + b} end)
     |> Enum.take(n + 1)
     |> Enum.map(&elem(&1, 0))
     |> Enum.sum()) *
      4
  end
end
