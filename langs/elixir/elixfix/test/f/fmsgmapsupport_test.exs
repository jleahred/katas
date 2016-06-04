defmodule FMsgMapSupportTest do
  use ExUnit.Case
  doctest FMsgMapSupport


  @msg_map2test FMsgParse.parse_string_test(
        "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|"<>
        "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
        "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
        .parsed.msg_map


  test "check tag value OK" do
    assert  FMsgMapSupport.check_tag_value(8, "FIX.4.4", @msg_map2test, []) == []
  end

  test "check tag value wrong" do
    assert  FMsgMapSupport.check_tag_value(8, "FIX.4.2", @msg_map2test, []) ==
            [" invalid tag value tag: 8  received: FIX.4.4, expected  FIX.4.2"]
  end

  test "check tag value OK prev errors" do
    assert  FMsgMapSupport.check_tag_value(8, "FIX.4.4", @msg_map2test,
            ["prev error"]) ==  ["prev error"]
  end

  test "check tag value wrong prev errors" do
    assert  FMsgMapSupport.check_tag_value(8, "FIX.4.1", @msg_map2test,
            ["prev error"]) == ["prev error",
            " invalid tag value tag: 8  received: FIX.4.4, expected  FIX.4.1"]
  end

  test "get integer value OK" do
    assert  FMsgMapSupport.get_tag_value_mandatory_int(11, @msg_map2test)
            == {:ok, 13346}
  end

  test "get integer value string field" do
    assert  FMsgMapSupport.get_tag_value_mandatory_int(56, @msg_map2test)
            == {:error, "invalid val on tag 56"}
  end


end
