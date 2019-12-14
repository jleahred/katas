defmodule Jle2Web.PlugLog do
  require Logger

  def init(opts) do
    Logger.info("opts____ #{inspect(opts, pretty: true)}")
    opts
  end

  def call(conn, opts) do
    Logger.info("conn____ #{inspect(conn, pretty: true)}")
    Logger.info("opts____ #{inspect(opts, pretty: true)}")
    Logger.info("from____ #{inspect(conn.remote_ip, pretty: true)}")
    Logger.info("request_path____ #{inspect(conn.request_path, pretty: true)}")
    Logger.info("params____ #{inspect(conn.params, pretty: true)}")
    Logger.info("path_params____ #{inspect(conn.path_params, pretty: true)}")

    Logger.info(
      "user_agent____ #{
        inspect(
          conn.req_headers
          |> Enum.filter(fn {k, _v} -> k == "user-agent" end)
          |> (fn [{_k, v}] -> v end).(),
          pretty: true
        )
      }"
    )

    Logger.info("session____ #{inspect(conn.private.plug_session, pretty: true)}")
    conn
  end
end
