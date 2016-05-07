defmodule MsgParse do

@doc ~S"""
Convert a string with a FIX message into
a map(string, string)

## Examples

    iex> fix_map = MsgParse.fix_msg2map("1=one\02=two\03=three")
    %{"1" => "one", "2" => "two", "3" => "three"}
    iex> fix_map["2"]
    "two"
    iex> fix_map["3"]
    "three"

"""
def fix_msg2map(msg) do
  msg |> String.split("\0")
      |> Enum.map(&(String.split &1, "="))
      |> Enum.map(&(List.to_tuple &1))
      |> Enum.into(%{})
end


@doc ~S"""
Convert a string with a FIX message into
a map(int, string)

## Examples

    iex> fix_map = MsgParse.fix_msg2mapi("1=one\02=two\03=three")
    %{1 => "one", 2 => "two", 3 => "three"}
    iex> fix_map[2]
    "two"
    iex> fix_map[3]
    "three"

"""
def fix_msg2mapi(msg) do
  msg |> String.split("\0")
      |> Enum.map(&(String.split &1, "="))
      |> Enum.map(&(List.to_tuple &1))
      |> Enum.map(fn({k, v}) -> { String.to_integer(k), v } end)
      |> Enum.into(%{})
end


def test_performance do
  num_msg = 100_000

  msg = "8=FIX.4.1\09=154\035=6\049=BRKR\056=INVMGR\034=236\052=19980604-07:58:48\023=115685\028=N\055=SPMI.MI\054=2\027=200000\044=10100.000000\025=H\010=159\08=FIX.4.1\09=90\035=0\049=INVMGR\056=BRKR\034=236\052=19980604-07:59:30\010=225"

  funt = fn -> Enum.each(1..num_msg,  fn(_) -> fix_msg2mapi msg end) end

  secs = :timer.tc(funt)
  |> elem(0)
  |> Kernel./(1_000_000)

  "#{num_msg/secs} msg/sec"

end

end
