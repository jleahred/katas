defmodule MsgParse do

  defmodule PS do
    defstruct map_msg: %{}, body_length: 0, check_sum: 0
  end

  defmodule EndMessage do
    defstruct pars_st: %PS{}
  end

  defmodule PartTag do
    defstruct pars_st: %PS{}, chunk: ""
  end

  defmodule PartVal do
    defstruct pars_st: %PS{}, tag: 0, chunk: ""
  end



  @doc ~S"""
  Add a new char msg to already received chunk

  First parameter is status, the second one is the character to be added.

  It will return the new status


  Status could be...

  * PartTag -> reading a tag
  * PartVal -> reading a value (after =)
  * EndMessage -> a message has been completed
  * All status has the field pars_st of type PS (parsed status)



  ###  Examples

    iex> stpars = MsgParse.add_char(%MsgParse.EndMessage{}, ?8)
    %MsgParse.PartTag{chunk: "8", pars_st: %MsgParse.PS{body_length: 1, check_sum: 56, map_msg: %{}}}
    iex> stpars = MsgParse.add_char(stpars, ?=)
    %MsgParse.PartVal{chunk: "", pars_st: %MsgParse.PS{body_length: 0, check_sum: 117, map_msg: %{}}, tag: 8}
    iex> stpars = MsgParse.add_char(stpars, ?F)
    %MsgParse.PartVal{chunk: "F", pars_st: %MsgParse.PS{body_length: 0, check_sum: 187, map_msg: %{}}, tag: 8}
    iex> stpars = %MsgParse.PartVal{chunk: "FIX4.", pars_st: %MsgParse.PS{body_length: 0, check_sum: 190, map_msg: %{}}, tag: 8}
    iex> stpars = MsgParse.add_char(stpars, ?4)
    %MsgParse.PartVal{chunk: "FIX4.4", pars_st: %MsgParse.PS{body_length: 0, check_sum: 242, map_msg: %{}}, tag: 8}
    iex> stpars = MsgParse.add_char(stpars, 1)
    %MsgParse.PartTag{chunk: "", pars_st: %MsgParse.PS{body_length: 0, check_sum: 243, map_msg: %{8 => "FIX4.4"}}}
    iex> stpars = MsgParse.add_char(stpars, ?9)
    %MsgParse.PartTag{chunk: "9", pars_st: %MsgParse.PS{body_length: 1, check_sum: 44, map_msg: %{8 => "FIX4.4"}}}
    iex> stpars = MsgParse.add_char(stpars, ?=)
    %MsgParse.PartVal{chunk: "", pars_st: %MsgParse.PS{body_length: 0, check_sum: 105, map_msg: %{8 => "FIX4.4"}}, tag: 9}
    iex> stpars = MsgParse.add_char(stpars, ?1)
    %MsgParse.PartVal{chunk: "1", pars_st: %MsgParse.PS{body_length: 0, check_sum: 154, map_msg: %{8 => "FIX4.4"}}, tag: 9}
    iex> stpars = MsgParse.add_char(stpars, ?2)
    %MsgParse.PartVal{chunk: "12", pars_st: %MsgParse.PS{body_length: 0, check_sum: 204, map_msg: %{8 => "FIX4.4"}}, tag: 9}
    iex> stpars = MsgParse.add_char(stpars, ?2)
    %MsgParse.PartVal{chunk: "122", pars_st: %MsgParse.PS{body_length: 0, check_sum: 254, map_msg: %{8 => "FIX4.4"}}, tag: 9}
    iex> stpars = MsgParse.add_char(stpars, 1)
    %MsgParse.PartTag{chunk: "", pars_st: %MsgParse.PS{body_length: 0, check_sum: 255, map_msg: %{8 => "FIX4.4", 9 => "122"}}}
"""


  def add_char(%EndMessage{ pars_st: _ps }, ch)  do
      add_char(%PartTag{}, ch)
  end

  def add_char(%PartTag{pars_st: ps, chunk: chunk}, ?=)  do
      #IO.puts chunk
      tag = String.to_integer(chunk)
      body_length = if tag == 10 or tag == 8 or tag == 9 do
                      ps.body_length - String.length(chunk)
                    else
                      ps.body_length+1
                    end
      check_sum = rem(
                    if tag == 10 do
                      ps.check_sum + 256 + 256 - ?1 -?0
                    else
                      ps.check_sum + ?=
                    end,
                  256)

      %PartVal {
        pars_st: %PS{ps | body_length: body_length, check_sum: check_sum},
        tag: tag,
        chunk: ""
      }
  end

  def add_char(%PartTag{pars_st: ps, chunk: chunk}, ch)  do
      %PartTag {
        pars_st: %PS{ ps |
                      body_length: ps.body_length+1,
                      check_sum: rem(ps.check_sum + ch, 256)},
        chunk: chunk <> <<ch>>
      }
  end

  def add_char(%PartVal{pars_st: ps, tag: 10, chunk: _}, 1)  do
      #IO.puts "parsed full message"
      %EndMessage { pars_st: ps }
  end

  def add_char(%PartVal{pars_st: ps, tag: tag, chunk: chunk}, 1)  do
    %PartTag {
      pars_st: %PS{ ps |
                    body_length: ps.body_length + if(tag==8 or tag==9, do: 0, else: 1),
                    check_sum: rem(ps.check_sum + 1, 256),
                    map_msg: Map.put(ps.map_msg, tag, chunk)},
      chunk: ""
    }


  end

  def add_char(%PartVal{pars_st: ps, tag: tag, chunk: chunk}, ch)  do
    #IO.puts "#{tag}  #{chunk<> <<ch>>}"
    %PartVal {
      pars_st: %PS{ ps |
                    body_length: ps.body_length + if(tag==8 or tag==9 or tag==10, do: 0, else: 1),
                    check_sum: if(tag != 10, do: rem(ps.check_sum + ch, 256), else: ps.check_sum)},
      tag: tag,
      chunk: chunk <> <<ch>>
    }
  end



  def test_parse_string do
      msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
      msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))

      msg_list |> Enum.reduce(%EndMessage{}, &(add_char(&2, &1)))
  end

  def test_parse_string_perf do
      msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
      msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))

      num_msg = 100_000

      funt = fn -> Enum.each(1..num_msg,
          fn(_) -> msg_list |> Enum.reduce(%EndMessage{}, &(add_char(&2, &1))) end)
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



end
