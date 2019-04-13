defmodule PhoenixKatas.IndexContent do
  @index [
    {"search?txt=a", "testing:search"},
    {"/", "index"},
    {"search", "search on index"},
    {"test", "testing"}
  ]

  @index_uc @index |> Enum.map(fn {i, d} -> {{i, d}, {String.upcase(i), String.upcase(d)}} end)

  def search(txt) do
    String.split(txt)
    |> Enum.reduce(@index_uc, fn w, acc -> filter_word(acc, String.upcase(w)) end)
    |> Enum.map(fn {orig, _} -> orig end)
  end

  defp filter_word(list, w) do
    list
    |> Enum.filter(fn {_orig, {idx, desc}} ->
      String.contains?(idx, w) or String.contains?(desc, w)
    end)
  end
end
