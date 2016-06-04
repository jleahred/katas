defmodule  FSessionReceiver.Support  do
@moduledoc false

  #import FSessionReceiver
  import FMsgMapSupport, only: [check_tag_value: 3,
                                get_tag_value_mandatory_int: 2,
                                check_mandatory_tags: 2]


  def  process_logon(status, msg_map) do
    {_, errors} =
      { msg_map , []}
      |>  check_mandatory_tags([96, 98, 108])
      |>  check_tag_value(96, status.password)  # check password
      |>  check_tag_value(98, 0)                # no encripytion


    #errors = [FMsgParse.check_mandatory_tags(msg_map, [96, 98, 108])]

    # check password
    #errors = check_tag_value(96, status.password, msg_map, errors)
    # no encription
    #errors = check_tag_value(98, "0", msg_map, errors)
    # reset sequence
    { msg_seq, errors } =
        case Map.get(msg_map, 141, nil)  do
            "N"   ->   { status.msg_seq_num, errors }
            "Y"   ->   { 0,                  errors }
            nil ->     { status.msg_seq_num, errors }
            other ->   { status.msg_seq_num, errors ++
                ["Invalid value on tag 141 (ResetSeqNumFlag) rec: #{other}"] }
        end

    { hearbeat, errors } =
        case get_tag_value_mandatory_int(108, msg_map)  do
            { :ok,    val }   ->   { val, errors }
            { :error, desc }  ->   { 0, errors ++
                              ["Invalid value on tag 108 (Hearbeat) #{desc}"] }
        end


    if errors == []  do
      {  %FSessionReceiver.Status { status | state:            :login_ok,
                            heartbeat_interv: hearbeat,
                            msg_seq_num:      msg_seq},
          if msg_seq == 0 do
              :reset_sequence
          else
              :none
          end
      }
    else
      {  %FSessionReceiver.Status { status | msg_seq_num:      msg_seq},
          reject_msg: errors
      }
    end
  end

end
