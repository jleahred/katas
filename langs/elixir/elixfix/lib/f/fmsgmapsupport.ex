defmodule  FMsgMapSupport do
  @moduledoc """
Support functions to deal with fix msg_map

    """




@doc """
Check if tag has value on msg_map

It will receive previous errors and will add current error to this list if
necessary.

  iex> msg_map = FMsgParse.parse_string_test(
  ...> "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|"<>
  ...> "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
  ...> "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
  ...> .parsed.msg_map
  iex> FMsgMapSupport.check_tag_value(8, "FIX.4.4", msg_map, [])
  []

  iex> msg_map = FMsgParse.parse_string_test(
  ...> "8=FIX.4.1|9=122|35=D|34=215|49=CLIENT12|"<>
  ...> "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
  ...> "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
  ...> .parsed.msg_map
  iex> FMsgMapSupport.check_tag_value(8, "FIX.4.4", msg_map, [])
  [" invalid tag value tag: 8  received: FIX.4.1, expected  FIX.4.4"]

"""
def check_tag_value(tag, value, msg_map, errors) do
  if msg_map[tag] != value do
    errors ++ [ " invalid tag value tag: #{tag}  received: #{msg_map[tag]}, " <>
                "expected  #{value}"]
  else
    errors
  end
end


@doc """
Return int from tag

It will return

> { :ok, int_val }
> { :error, description }


    iex> msg_map = FMsgParse.parse_string_test(
    ...> "8=FIX.4.1|9=122|35=D|34=215|49=CLIENT12|"<>
    ...> "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
    ...> "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
    ...> .parsed.msg_map
    iex> FMsgMapSupport.get_tag_value_mandatory_int(11, msg_map)
    {:ok, 13346}
    iex> FMsgMapSupport.get_tag_value_mandatory_int(56, msg_map)
    {:error, "invalid val on tag 56"}

"""
def get_tag_value_mandatory_int(tag, msg_map)  do
  try do  # better performance than  Integer.parse
    { :ok, String.to_integer(msg_map[tag]) }
  rescue
    _  ->   {:error, "invalid val on tag #{tag}"}
  end
end


end
