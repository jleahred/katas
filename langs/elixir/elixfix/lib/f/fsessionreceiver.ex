defmodule  FSessionReceiver  do


  defmodule Status do
    @doc """

    """
    defstruct   state:            :waitting_login,   # :login_ok
                fix_version:      "",
                sender_comp_id:   "",
                target_comp_id:   "",
                password:         "",
                heartbeat_interv:  0,
                msg_seq_num:       0    # returning 0 means reset seq numb
  end






  defp check_tag_value(tag, value, msg_map, errors) do
    if msg_map[tag] != value do
      errors ++ [ " invalid tag value tag: #{tag}  received: #{value}, " <>
                  "expected  val: #{msg_map[tag]}"]
    else
      errors
    end
  end

  defp get_tag_value_optional(tag, msg_map) do
    if Map.has_key?(msg_map, tag)  do
        msg_map[tag]
    else
        :none
    end
  end

  defp get_tag_value_mandatory_int(tag, msg_map)  do
    try do  # better performance than  Integer.parse
      { :ok, String.to_integer(msg_map[tag]) }
    rescue
      _  ->   {:error, "invalid val on tag #{tag}"}
    end
  end




  @doc """
  Process message and return new status and action

  It will return the new status and action to be done

  Possible actions are:

      * :none
      * :reset_sequence
      * reject_msg: desctiption
  """
  def process_message(status, msg_map) do
      errors = check_tag_value(8,  status.fix_version,    msg_map, [])
      errors = check_tag_value(49, status.sender_comp_id, msg_map, errors)
      errors = check_tag_value(56, status.sender_comp_id, msg_map, errors)
      if errors == []  do
          status = %Status{ status |  msg_seq_num: status.msg_seq_num+1 }
          get_func_proc_msg(msg_map[35]).(status, msg_map)
      else
          {  %Status { status |  msg_seq_num: status.msg_seq_num+1},
             reject_msg: errors
          }
      end
  end



  defp get_func_proc_msg(msg_type) do
      case msg_type do
          "A"   ->  &logon/2
          "5"   ->  &logout/2
          "0"   ->  &heartbeat/2
          "1"   ->  &test_request/2
          "4"   ->  &sequence_reset/2
          "2"   ->  &resend_request/2
          "3"   ->  &session_level_reject/2
          _     ->  &not_session_message/2
      end
  end

  defp logon(status, msg_map)  do
    errors = [FMsgParse.check_mandatory_tags(msg_map, [96, 98, 108])]

    # check password
    errors = check_tag_value(96, status.password, msg_map, errors)
    # no encription
    errors = check_tag_value(98, "0", msg_map, errors)
    # reset sequence
    { msg_seq, errors } =
        case get_tag_value_optional(141, msg_map)  do
            "N"   ->   { status.msg_seq_num, errors }
            "Y"   ->   { 0,                  errors }
            :none ->   { status.msg_seq_num, errors }
            other ->   { status.msg_seq_num, errors ++
                ["Invalid value on tag 141 (ResetSeqNumFlag) rec: #{other}"] }
        end

    { hearbeat, errors } =
        case get_tag_value_mandatory_int(108, msg_map)  do
            { :ok,    val }   ->   { val, [] }
            { :error, desc }  ->   { 0, errors ++
                              ["Invalid value on tag 108 (Hearbeat) #{desc}"] }
        end


    if errors == []  do
      {  %Status { status | state:            :login_ok,
                            heartbeat_interv: hearbeat,
                            msg_seq_num:      msg_seq},
          if msg_seq == 0 do
              :reset_sequence
          else
              :none
          end
      }
    else
      {  %Status { status | heartbeat_interv: hearbeat,
                            msg_seq_num:      msg_seq},
          reject_msg: errors
      }
    end


  end




  defp logout(status, msg_map)  do

  end

  defp heartbeat(status, msg_map)  do

  end

  defp test_request(status, msg_map)  do

  end

  defp sequence_reset(status, msg_map)  do

  end

  defp resend_request(status, msg_map)  do

  end

  defp session_level_reject(status, msg_map)  do

  end

  defp not_session_message(status, msg_map)  do

  end

end
