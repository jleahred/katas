defmodule  Session  do

  def process_message(msg_map) do
      # update last received sequence
      get_func_proc_msg(msg_map[35]).(msg_map)
  end

  defp get_func_proc_msg(msg_type) do
      case msg_type do
          "A"   ->  &logon/1  
          "5"   ->  &logout/1
          "0"   ->  &heartbeat/1
          "1"   ->  &test_request/1
          "4"   ->  &sequence_reset/1
          "2"   ->  &resend_request/1
          "3"   ->  &session_level_reject/1
          _     ->  &not_session_message/1
      end
  end

  defp logon(msg_map)  do

  end

  defp logout(msg_map)  do

  end

  defp heartbeat(msg_map)  do

  end

  defp test_request(msg_map)  do

  end

  defp sequence_reset(msg_map)  do

  end

  defp resend_request(msg_map)  do

  end

  defp session_level_reject(msg_map)  do

  end

  defp not_session_message(msg_map)  do

  end

end
