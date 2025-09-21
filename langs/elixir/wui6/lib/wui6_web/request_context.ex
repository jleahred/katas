defmodule Wui6Web.RequestContext do
  @moduledoc """
  Builds per-request snapshots used to track session activity metadata.
  """

  import Plug.Conn, only: [get_req_header: 2]

  @spec snapshot(Plug.Conn.t()) :: %{data: map(), timestamp: NaiveDateTime.t()}
  def snapshot(conn) do
    timestamp = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    %{
      data: %{
        "ip" => ip_to_string(conn.remote_ip),
        "user_agent" => header(conn, "user-agent"),
        "accept_language" => header(conn, "accept-language"),
        "referer" => header(conn, "referer"),
        "at" => NaiveDateTime.to_iso8601(timestamp)
      },
      timestamp: timestamp
    }
  end

  defp header(conn, name) do
    conn
    |> get_req_header(name)
    |> List.first()
  end

  defp ip_to_string(nil), do: nil

  defp ip_to_string({_, _, _, _} = ip), do: :inet.ntoa(ip) |> to_string()

  defp ip_to_string({_, _, _, _, _, _, _, _} = ip), do: :inet.ntoa(ip) |> to_string()

  defp ip_to_string(other) when is_binary(other), do: other

  defp ip_to_string(_), do: nil
end
