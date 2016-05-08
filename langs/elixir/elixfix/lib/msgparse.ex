defmodule MsgParse do


@doc ~S"""
Add a new char msg to already received chunk

First parameter is status, the second one is the character to be added.

It will return the new status


Status could be...

* { :end_msg, map_msg, {body_length, check_sum} } when a message is completed
* { :partial_tag, chunk, map_msg, {body_length, check_sum} }  reading tag
* { :partial_val, tag, part_val, map_msg, {body_length, check_sum} }  reading val


###  Examples

    iex> MsgParse.add_char({ :partial_tag, "123" }, ?4)
    {:partial_tag, "1234"}

    iex> MsgParse.add_char({ :partial_tag, "123" }, ?=)
    {:partial_val, 123, ""}

    iex> MsgParse.add_char({ :partial_val, 123, "abc" }, 0)
    {:full_tag_val, 123, "abc"}

    iex> MsgParse.add_char({ :partial_val, 123, "ab" }, ?c)
    {:partial_tag, 123, "abc"}
"""


def add_char({ :end_msg, _map_msg, {_body_length, _check_sum} }, ch) do
    add_char({ :partial_tag, "", %{}, {0, 0} }, ch)
end

def add_char({ :partial_tag, chunk, map_msg, {body_length, check_sum} }, ?=) do
    #IO.puts chunk
    tag = String.to_integer(chunk)
    { :partial_val,
      tag,
      "",
      map_msg,
      { if tag == 10 or tag == 8 or tag == 9 do
          body_length - String.length(chunk)
        else
          body_length+1
        end,
        rem(
            if tag == 10 do
              check_sum + 256 + 256 - ?1 -?0
            else
              check_sum + ?=
            end,
        256)
      }
    }
end

def add_char({ :partial_tag, chunk, map_msg, {body_length, check_sum} }, ch) do
    { :partial_tag,
      chunk <> <<ch>>,
      map_msg,
      {body_length+1, rem(check_sum + ch, 256)}
    }
end

def add_char({ :partial_val, 10, _val, map_msg, {body_length, check_sum}}, 1) do
    #IO.puts "parsed full message"
    { :end_msg,
      map_msg,
      {body_length, rem(check_sum, 256)}
    }
end

def add_char({ :partial_val, tag, _val, map_msg, {body_length, check_sum}}, 1) do
    { :partial_tag,
      "",
      map_msg,
      if(tag==8 or tag==9) do
        { body_length, rem(check_sum + 1, 256)}
      else
        { body_length+1, rem(check_sum + 1, 256)}
      end
    }
end

def add_char({ :partial_val, tag, chunk, map_msg, {body_length, check_sum} }, ch) do
    { :partial_val,
      tag,
      chunk <> <<ch>>,
      map_msg,
      { if tag==8 or tag==9 or tag==10 do
          body_length
        else
          body_length+1
        end,
        if tag != 10 do
          rem(check_sum + ch, 256)
        else
          check_sum
        end
      }
    }
end




def test_parse_string do
    msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
    msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))

    msg_list |> Enum.reduce({:end_msg, %{}, {0, 0}}, &(add_char(&2, &1)))
end

def test_parse_2string do
    msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
    msg_list = String.to_char_list(String.replace(msg <> msg, "|", <<1>>))

    msg_list |> Enum.reduce({:end_msg, %{}, {0, 0}}, &(add_char(&2, &1)))
end




@doc ~S"""

## Examples

    _iex> fix_map = MsgParse.test_parse_string
"""
def test_parse_string_perf do
    msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
    msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))

    num_msg = 100_000

    funt = fn -> Enum.each(1..num_msg,
        fn(_) -> msg_list |> Enum.reduce({:end_msg, %{}, {0, 0}}, &(add_char(&2, &1))) end)
      end

    secs = :timer.tc(funt)
    |> elem(0)
    |> Kernel./(1_000_000)

    IO.puts "total time #{secs} sec"
    IO.puts "#{num_msg/secs} msg/sec"
end
















@doc ~S"""
Convert a string with a FIX message into
a map(int, string)

## Examples

    iex> fix_map = MsgParse.fix_body2map("1=one\02=two\03=three")
    %{1 => "one", 2 => "two", 3 => "three"}
    iex> fix_map[2]
    "two"
    iex> fix_map[3]
    "three"
"""
def fix_body2map(msg) do
  msg |> String.split("\0")
      |> Enum.map(&(String.split &1, "="))
      |> Enum.map(&(List.to_tuple &1))
      |> Enum.map(fn({k, v}) -> { String.to_integer(k), v } end)
      |> Enum.into(%{})
end


def test_performance_body2map do
  num_msg = 100_000

  msg = "8=FIX.4.1\19=154\135=6\149=BRKR\156=INVMGR\134=236\152=19980604-07:58:48\123=115685\128=N\155=SPMI.MI\154=2\127=200000\144=10100.000000\125=H\110=159\18=FIX.4.1\19=90\135=0\149=INVMGR\156=BRKR\134=236\152=19980604-07:59:30\110=225"

  funt = fn -> Enum.each(1..num_msg,  fn(_) -> fix_body2map msg end) end

  secs = :timer.tc(funt)
  |> elem(0)
  |> Kernel./(1_000_000)

  "#{num_msg/secs} msg/sec"

end


def test_perf_concat_string do

  num_msg = 1_000

  funt =
    fn -> Enum.each(1..num_msg,
        fn(_) ->  Enum.reduce(1..1000, "", fn(_i, acc)-> acc <> <<?a>> end) end
        ) end
    secs = :timer.tc(funt)
    |> elem(0)
    |> Kernel./(1_000_000)

    IO.puts "total time #{secs} sec"
    IO.puts "#{num_msg/secs} msg/sec"

end

def test_perf_concat_list do

  num_msg = 1_000

  funt = fn -> Enum.each(1..num_msg,
      fn(_) -> Enum.reduce(1..1000, [], fn(_i, acc)-> [?a | acc] end) end
    ) end

    secs = :timer.tc(funt)
    |> elem(0)
    |> Kernel./(1_000_000)

    IO.puts "total time #{secs} sec"
    IO.puts "#{num_msg/secs} msg/sec"

end



end
