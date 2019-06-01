defmodule JLEWeb.MainPageView do
  use JLEWeb, :view

  defp search(txt) do
    String.split(txt)
    |> Enum.reduce(JLE.IndexContent.all(), fn w, acc ->
      filter_word(acc, String.upcase(w))
    end)
    |> Enum.map(fn {orig, _} -> orig end)
  end

  defp filter_word(list, w) do
    list
    |> Enum.filter(fn {_orig, {idx, desc}} ->
      String.contains?(idx, w) or String.contains?(desc, w)
    end)
  end
end
