defmodule DuplicateCount do
  def count(str) do
    # str
    # |> String.downcase()
    # |> String.graphemes()
    # |> Enum.reduce(%{}, fn l, acc -> Map.update(acc, l, 1, &(&1 + 1)) end)
    # |> Enum.reduce(0, fn {_, count}, acc -> if count > 1, do: acc + 1, else: acc end)

    str
    |> String.graphemes()
    |> Enum.group_by(& &1)
    |> Enum.reduce(0, fn {_, l}, acc -> if length(l) > 1, do: acc + 1, else: acc end)
  end
end
