defmodule FProcessLogonTest do
    use ExUnit.Case
    #doctest

    test "Process logon OK" do
        status = %Session.Status{
            connect_role:     :acceptor,
            status:           :waitting_login,
            password:         "1234",
            me_comp_id:       "ACCEPTOR",
            other_comp_id:    "INITIATOR"
        }

        logon = %{
            :MsgSeqNum => 1,
            :SenderCompID => "INITIATOR",
            :TargetCompID => "ACCEPTOR",
            :Password => "1234",
            :EncryptMethod => "0",
            :HeartBtInt => "60",
            :ResetSeqNumFlag => "N"
        }

        expected_status = %Session.Status{
            connect_role:     :acceptor,
            status:           :login_ok,
            password:         "1234",
            me_comp_id:       "ACCEPTOR",
            other_comp_id:    "INITIATOR",
            heartbeat_interv: 60,
            receptor:         %Session.Receptor{msg_seq_num: 1},
            sender:           %Session.Sender{msg_seq_num: 1}
        }
        confirm_login = %{
            :MsgSeqNum => 1,
            :SenderCompID => "ACCEPTOR",
            :TargetCompID => "INITIATOR",
            :HeartBtInt => 60
        }


        {final_status, actions} = FProcessLogon.process_logon(status, logon)
        assert  final_status == expected_status
        assert  actions == [send_message: confirm_login]
    end

end
