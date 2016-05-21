defmodule MsgParse do


  defprotocol Status do
    def increase_position(status)
  end

  #######################################################3
  defmodule MsgInfo do
    defstruct   map_msg:    %{},
                body_length:  0,
                check_sum:    0,
                num_tags:     0,
                orig_msg:    "",
                errors:      [],  #{pos, description}
                position:     0
  end


  def msginf_incpos(msg_info) do
     %MsgInfo {
        msg_info | position: msg_info.position + 1
     }
  end

  #######################################################3
  defmodule StFullMessage do
    defstruct msg_inf: %MsgInfo{}
  end

  defimpl Status, for: StFullMessage do
    def increase_position(status) do
      %StFullMessage {
          status  |
          msg_inf: MsgParse.msginf_incpos(status.msg_inf)
      }
    end
  end

  #######################################################3
  defmodule StPartTag do
    defstruct msg_inf: %MsgInfo{}, chunk: ""
  end

  defimpl Status, for: StPartTag do
    def increase_position(status) do
      %StPartTag {
          status  |
          msg_inf: MsgParse.msginf_incpos(status.msg_inf)
      }
    end
  end

  #######################################################3
  defmodule StPartVal do
    defstruct msg_inf: %MsgInfo{}, tag: 0, chunk: ""
  end

  defimpl Status, for: StPartVal do
    def increase_position(status) do
      %StPartVal {
          status  |
          msg_inf: MsgParse.msginf_incpos(status.msg_inf)
      }
    end
  end




  defp add_error_list(error_list, new_error_desc)  do
    cond do
      new_error_desc != "" ->
        cond  do
          Enum.count(error_list) <  3 ->   error_list ++ [new_error_desc]
          Enum.count(error_list) == 3 ->   error_list ++ [new_error_desc]
                                                      ++ ["too may errors"]
          true                        ->   error_list
        end
      true ->  error_list
    end
  end


  @doc ~S"""
  Add a new char msg to already received chunk

  First parameter is status, the second one is the character to be added.

  It will return the new status


  Status could be...

  * StPartTag -> reading a tag
  * StPartVal -> reading a value (after =)
  * StFullMessage -> a message has been completed
  * All status has the field msg_inf of type MsgInfo (parsed status)



  ###  Examples

    iex> msg =  "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|" <>
    ...>        "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|" <>
    ...>        "44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
    iex> msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))
    iex> msg_list |> Enum.reduce(%MsgParse.StFullMessage{}, &(MsgParse.add_char(&2, &1)))
    %MsgParse.StFullMessage{msg_inf: %MsgParse.MsgInfo{body_length: 122,
      check_sum: 72, errors: [],
      map_msg: %{1 => "Marcel", 8 => "FIX.4.4", 9 => "122", 10 => "072",
        11 => "13346", 21 => "1", 34 => "215", 35 => "D", 40 => "2", 44 => "5",
        49 => "CLIENT12", 52 => "20100225-19:41:57.316", 54 => "1", 56 => "B",
        59 => "0", 60 => "20100225-19:39:52.020"}, num_tags: 15,
      orig_msg: "8=FIX.4.4^9=122^35=D^34=215^49=CLIENT12^52=20100225-19:41:57.316^56=B^1=Marcel^11=13346^21=1^40=2^44=5^54=1^59=0^60=20100225-19:39:52.020^10=072", position: 144}}

      iex> MsgParse.add_char(%MsgParse.StFullMessage{}, ?8)
      %MsgParse.StPartTag{chunk: "8",
       msg_inf: %MsgParse.MsgInfo{body_length: 1, check_sum: 56, errors: [],
        map_msg: %{}, num_tags: 0, orig_msg: "8"}}

"""

  def add_char(status, char) do
    _add_char(Status.increase_position(status), char)
  end


  defp _add_char(%StFullMessage{ msg_inf: msg_inf }, 1)  do
    %StFullMessage{
      msg_inf: %MsgInfo
               { msg_inf
               | errors: add_error_list(msg_inf.errors,
                 "Invalid SOH after full message  recieved #{msg_inf.orig_msg}")
              }
    }
  end

  defp _add_char(%StFullMessage{ msg_inf: _msg_inf }, ch)  do
      _add_char(%StPartTag{}, ch)
  end

  defp _add_char(%StPartTag{msg_inf: msg_inf, chunk: chunk}, ?=)  do
      try do  # better performance than  Integer.parse
        tag = String.to_integer(chunk)
        body_length = if tag == 10 or tag == 8 or tag == 9 do
                       msg_inf.body_length - String.length(chunk)
                     else
                       msg_inf.body_length+1
                     end
        check_sum = rem(
                     if tag == 10 do
                       msg_inf.check_sum + 256 + 256 - ?1 -?0
                     else
                       msg_inf.check_sum + ?=
                      end,
                    256)

        %StPartVal {
          msg_inf: %MsgInfo
                    { msg_inf
                    | body_length:  body_length,
                      check_sum:    check_sum,
                      orig_msg:     msg_inf.orig_msg <> "=",
                      errors:       if(chunk=="")  do
                                      add_error_list(msg_inf.errors,
                                      "Ending empty tag on #{msg_inf.orig_msg}")
                                    else msg_inf.errors
                                    end
                    },
          tag: tag,
          chunk: ""
        }
      rescue
        _ ->
          %StPartVal {
            msg_inf: %MsgInfo
                       { msg_inf
                       | orig_msg:    msg_inf.orig_msg <> "=",
                         errors:      add_error_list(msg_inf.errors,
                                "Error tag value #{chunk} on #{msg_inf.orig_msg}")
                       },
            tag: 0,
            chunk: ""
          }
      end
  end


  defp _add_char(%StPartTag{msg_inf: msg_inf, chunk: chunk}, 1)  do
      %StPartTag {
        msg_inf: %MsgInfo
                     { msg_inf
                     | errors:    add_error_list(msg_inf.errors,
                                  "Invalid SOH on #{chunk} waiting for tag"),
                       orig_msg:  msg_inf.orig_msg <> "^"
                     },
        chunk: ""
      }
  end

  defp _add_char(%StPartTag{msg_inf: msg_inf, chunk: chunk}, ch)  do
      %StPartTag {
        msg_inf: %MsgInfo
                      { msg_inf |
                        body_length: msg_inf.body_length+1,
                        check_sum: rem(msg_inf.check_sum + ch, 256),
                        orig_msg:  msg_inf.orig_msg <> <<ch>>
                      },
        chunk: chunk <> <<ch>>
      }
  end

  defp _add_char(%StPartVal{msg_inf: msg_inf, tag: 10, chunk: chunk}, 1)  do
      bl =  try do
                String.to_integer(msg_inf.map_msg[9])
            rescue
              _  ->  -1
            end
      check_sum = try do
                      String.to_integer(chunk)
                  rescue
                    _  ->  -1
                  end
      error = cond do
          msg_inf.body_length != bl       ->
                "Incorrect body length  calculated: #{msg_inf.body_length} received #{bl}  #{msg_inf.orig_msg}"
          check_sum == -1    ->
                "Error parsing checksum #{msg_inf.orig_msg}  chs: #{chunk}"
          msg_inf.check_sum  != check_sum ->
                "Incorrect checksum calculated: #{msg_inf.check_sum} received #{check_sum}  #{msg_inf.orig_msg}  #{chunk}"
          true  -> ""
      end
      %StFullMessage {
          msg_inf: %MsgInfo { msg_inf
                            | map_msg: Map.put(msg_inf.map_msg, 10, chunk),
                              errors:  add_error_list(msg_inf.errors, error)
                            }
      }
  end

  defp _add_char(%StPartVal{msg_inf: msg_inf, tag: tag, chunk: chunk}, 1)  do
    error = cond do
        tag !=8 and msg_inf.num_tags == 0  ->  "First tag has to be 8"
        tag !=9 and msg_inf.num_tags == 1  ->  "Second tag has to be 9"
        tag ==8 and msg_inf.num_tags != 0  ->  "Tag 8 has to be on position 1"
        tag ==9 and msg_inf.num_tags != 1  ->  "Tag 9 has to be on position 2"
        true                               ->  ""
    end
    %StPartTag {
      msg_inf: %MsgInfo
                  { msg_inf
                  | body_length: msg_inf.body_length + if(tag==8 or tag==9, do: 0, else: 1),
                    check_sum: rem(msg_inf.check_sum + 1, 256),
                    map_msg: Map.put(msg_inf.map_msg, tag, chunk),
                    orig_msg:  msg_inf.orig_msg <> "^",
                    num_tags:  msg_inf.num_tags + 1,
                    errors:    add_error_list(msg_inf.errors, error)
                  },
      chunk: ""
    }


  end

  defp _add_char(%StPartVal{msg_inf: msg_inf, tag: tag, chunk: chunk}, ch)  do
    bl = if(tag==8 or tag==9 or tag==10, do: 0, else: 1)
    check_sum = if(tag != 10, do: rem(msg_inf.check_sum + ch, 256), else: msg_inf.check_sum)
    %StPartVal {
      msg_inf: %MsgInfo { msg_inf
                   | body_length: msg_inf.body_length + bl,
                     check_sum: check_sum,
                     orig_msg:  msg_inf.orig_msg <> <<ch>>
                   },
      tag: tag,
      chunk: chunk <> <<ch>>
    }
  end







  def test_parse_string do
      msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
      msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))

      msg_list |> Enum.reduce(%StFullMessage{}, &(_add_char(&2, &1)))
  end

  def test_parse_string_perf do
      msg = "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|"
      msg_list = String.to_char_list(String.replace(msg, "|", <<1>>))

      num_msg = 100_000

      funt = fn -> Enum.each(1..num_msg,
          fn(_) -> msg_list |> Enum.reduce(%StFullMessage{}, &(_add_char(&2, &1))) end)
        end

      secs = :timer.tc(funt)
      |> elem(0)
      |> Kernel./(1_000_000)

      IO.puts "total time #{secs} sec"
      IO.puts "#{num_msg/secs} msg/sec"
  end


end
