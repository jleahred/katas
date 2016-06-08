defmodule FSessionReceiverSupport_test do
  use ExUnit.Case
  import FSessionReceiver.Support

  test "process_logon_on_waitlog OK" do
      status = %FSessionReceiver.Status{
            state:            :waitting_login,   # :login_ok
            fix_version:      "FIX.4.4",
            sender_comp_id:   "SEND_TEST",
            target_comp_id:   "TARGET_TEST",
            password:         "PASS1234",
            heartbeat_interv:  30,
            msg_seq_num:       1
      }
      msg_map = FMsgParse.parse_string_test(
          "8=FIX.4.2‡9=71‡35=A‡34=1‡49=REMOTE‡52=20120330-19:23:32‡" <>
          "56=TT_ORDER‡96=PASS1234‡98=0‡108=30‡10=143‡", "‡").parsed.msg_map
      #IO.puts process_logon_on_waitlog(status, msg_map)
      assert process_logon_on_waitlog(status, msg_map) ==
          {%FSessionReceiver.Status{
                fix_version: "FIX.4.4",
                heartbeat_interv: 30,
                msg_seq_num: 1,
                password: "PASS1234",
                sender_comp_id: "SEND_TEST",
                state: :waitting_login,
                target_comp_id: "TARGET_TEST"},
                %{BeginString: "FIX.4.2", BodyLength: "71", CheckSum: "143",
                  EncryptMethod: "0", HeartBtInt: "30", MsgSeqNum: "1",
                  MsgType: "A", RawData: "PASS1234",
                  SenderCompID: "REMOTE", SendingTime: "20120330-19:23:32",
                  TargetCompID: "TT_ORDER"},
                nil}
  end

  test "process_logon_on_waitlog missing mandatory tags" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog invalid password" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog invalid encrypt method" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog. reset seq. no. seq > expected" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog. reset seq. no. seq < expected" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog. reset seq. yes. 1 OK" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog. reset seq. yes. !=1  wrong" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog. process_heart_beat. OK" do
    assert 1 + 1 == 2
  end

  test "process_logon_on_waitlog. process_heart_beat. invalid int" do
    assert 1 + 1 == 2
  end

end
