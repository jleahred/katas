defmodule Kata do
  def high(str) do
    str
    |> String.split()
    |> Enum.max_by(
      &score_word/1,
      fn -> "" end
    )
  end

  defp score_word(word) do
    word
    |> String.to_charlist()
    |> Enum.reduce(0, &(&1 + &2 - ?a + 1))
  end

  #   def high(str) do
  #     str
  #     |> String.split()
  #     |> Enum.reduce({"", 0}, fn word, {b_word, b_score} ->
  #       score_word = score(word)

  #       case score_word > b_score do
  #         true -> {word, score_word}
  #         false -> {b_word, b_score}
  #       end
  #     end)
  #     |> elem(0)
  #   end

  #   defp score(word) do
  #     word
  #     |> String.to_charlist()
  #     |> Enum.reduce(0, fn ch, acc -> acc + ch - 96 end)
  #   end
end
