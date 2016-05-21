defmodule MsgParseTest do
  use ExUnit.Case
  doctest MsgParse

  def process_string(string) do
    list = String.to_char_list(String.replace(string, "|", <<1>>))
    list |> Enum.reduce( %MsgParse.StFullMessage{},
                                      &(MsgParse.add_char(&2, &1)))
  end


  test "parsed full message OK" do
    result = process_string(
          "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|"<>
          "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
          "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")

    assert result.msg_inf.check_sum == 72
    assert result.msg_inf.body_length == 122
    assert result.msg_inf.errors == []
    assert result.msg_inf.map_msg[56] == "B"
  end

  test "parsed full message tag 8 not first" do
    result = process_string(
          "9=122|8=FIX.4.4|35=D|34=215|49=CLIENT12|"<>
          "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
          "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
    #IO.inspect result

    assert result.msg_inf.check_sum == 72
    assert result.msg_inf.body_length == 122
    assert result.msg_inf.errors == ["First tag has to be 8",
                                     "Second tag has to be 9"]
    assert result.msg_inf.map_msg[56] == "B"
  end


  test "parsed full message tag 9 not second" do
    result = process_string(
          "8=FIX.4.4|35=D|34=215|49=CLIENT12|9=122|"<>
          "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
          "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
    #IO.inspect result

    assert result.msg_inf.check_sum == 72
    assert result.msg_inf.body_length == 122
    assert result.msg_inf.errors == ["Second tag has to be 9",
                                     "Tag 9 has to be on position 2"
                                     ]
    assert result.msg_inf.map_msg[56] == "B"
  end

  test "parsed full message incorrect checksum" do
    result = process_string(
          "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|"<>
          "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
          "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=071|")
    #IO.inspect result

    assert result.msg_inf.check_sum == 72
    assert result.msg_inf.body_length == 122
    assert result.msg_inf.errors == ["Incorrect checksum calculated: 72 "<>
          "received 71  8=FIX.4.4^9=122^35=D^34=215^49=CLIENT12^"<>
          "52=20100225-19:41:57.316^56=B^1=Marcel^11=13346^21=1^40=2^"<>
          "44=5^54=1^59=0^60=20100225-19:39:52.020^10=071  071"
                                     ]
    assert result.msg_inf.map_msg[56] == "B"
  end

  test "parsed full message incorrect bodylength" do
    result = process_string(
          "8=FIX.4.4|9=121|35=D|34=215|49=CLIENT12|"<>
          "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
          "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
    #IO.inspect result

    assert result.msg_inf.check_sum == 71
    assert result.msg_inf.body_length == 122
    assert result.msg_inf.errors == ["Incorrect body length  calculated: 122 "<>
          "received 121  8=FIX.4.4^9=121^35=D^34=215^49=CLIENT12^"<>
          "52=20100225-19:41:57.316^56=B^1=Marcel^11=13346^21=1^40=2^44=5^"<>
          "54=1^59=0^60=20100225-19:39:52.020^10=072"
                                     ]
    assert result.msg_inf.map_msg[56] == "B"
  end

  test "partial val" do
    result = process_string("8=FIX.4.4|9=12")
    #IO.inspect result

    assert result ==
        %MsgParse.StPartVal{chunk: "12",
          msg_inf: %MsgParse.Parsed{body_length: 0, check_sum: 250, errors: [],
          map_msg: %{8 => "FIX.4.4"}, num_tags: 1, orig_msg: "8=FIX.4.4^9=12",
          position: 13}, tag: 9}
  end

  test "partial tag" do
    result = process_string(
          "8=FIX.4.4|9=121|35=D|34=215|49=CLIENT12|"<>
          "52=20100225-19:41:57.316|5")
    #IO.inspect result

    assert result ==
      %MsgParse.StPartTag{chunk: "5",
       msg_inf: %MsgParse.Parsed{body_length: 50, check_sum: 42, errors: [],
        map_msg: %{8 => "FIX.4.4", 9 => "121", 34 => "215", 35 => "D",
          49 => "CLIENT12", 52 => "20100225-19:41:57.316"}, num_tags: 6,
        orig_msg: "8=FIX.4.4^9=121^35=D^34=215^49=CLIENT12^52=20100225-19:41:57.316^5",
        position: 65}}
  end

  test "partial field =" do
    result = process_string("8=FIX.4.4|9=121|35=D|34=")
    #IO.inspect result

    assert result ==
      %MsgParse.StPartVal{chunk: "",
       msg_inf: %MsgParse.Parsed{body_length: 8, check_sum: 186, errors: [],
        map_msg: %{8 => "FIX.4.4", 9 => "121", 35 => "D"}, num_tags: 3,
        orig_msg: "8=FIX.4.4^9=121^35=D^34=", position: 23}, tag: 34}
    end

    test "invalid tag" do
      result = process_string(
            "8=FIX.4.4|9=121|35=D|34=215|49=CLIENT12|"<>
            "52=20100225-19:41:57.316|56a=B|1=Marcel|11=13346|"<>
            "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
      #IO.inspect result

      assert List.first(result.msg_inf.errors) ==
          {68, "invalid tag value 56a"}
    end

    test "emtpy tag" do
      result = process_string(
            "8=FIX.4.4|9=121|35=D|34=215|49=CLIENT12|"<>
            "52=20100225-19:41:57.316|=B|1=Marcel|11=13346|"<>
            "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072|")
      #IO.inspect result

      assert List.first(result.msg_inf.errors) ==
          {65, "invalid tag value "}
          "52=20100225-19:41:57.316^"
    end

    test "emtpy value" do
      result = process_string(
            "8=FIX.4.4|9=121|35=D|34=215|49=CLIENT12|"<>
            "52=20100225-19:41:57.316|56=|1=Marcel|11=13346|"<>
            "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=005|")
      #IO.inspect result

      assert result.msg_inf.check_sum == 5
      assert result.msg_inf.body_length == 121
      assert result.msg_inf.errors == []

    end

    test "SOH after full message" do
      result = process_string(
            "8=FIX.4.4|9=122|35=D|34=215|49=CLIENT12|"<>
            "52=20100225-19:41:57.316|56=B|1=Marcel|11=13346|"<>
            "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=072||")
      #IO.inspect result

      assert result.msg_inf.errors == [
                                  { 145,
                                    "Invalid SOH after full message recieved"}]
    end

    test "missing mandatory tag" do
      result = process_string(
            "8=FIX.4.4|9=117|35=D|34=215|49=CLIENT12|"<>
            "52=20100225-19:41:57.316|1=Marcel|11=13346|"<>
            "21=1|40=2|44=5|54=1|59=0|60=20100225-19:39:52.020|10=097|")

      assert result.msg_inf.check_sum == 97
      assert result.msg_inf.body_length == 117
      assert result.msg_inf.errors == ["missing tag 56."]
    end

end
