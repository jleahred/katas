defmodule TestSolutionSW do
  use ExUnit.Case

  @samples [
    {"man i need a taxi up to ubud", "taxi"},
    {"what time are we climbing up the volcano", "volcano"},
    {"take me to semynak", "semynak"},
    {"massage yes massage yes massage", "massage"},
    {"take two bintang and a dance please", "bintang"}
  ]

  test "Sample test" do
    Enum.map(@samples, fn {input, output} ->
      assert Kata.high(input) == output
    end)
  end

  test "Empty test" do
    assert Kata.high("") == ""
  end
end
