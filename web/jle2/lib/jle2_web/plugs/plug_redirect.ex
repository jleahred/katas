defmodule Jle2Web.PlugRedirect do
  require Logger
  use Jle2Web, :controller
  alias Jle2Web.Router.Helpers, as: Routes

  def init(opts) do
    opts
  end

  def call(conn, _opts) do
    if conn.request_path == "/redirect" do
      conn
      # |> put_flash(
      #   :error,
      #   "#{inspect(conn.path_info)}?#{inspect(conn.params, pretty: true)}"
      # )
      # |> put_session(:original_path, conn.path_info)
      # |> put_session(:original_params, conn.params)
      # |> assign(:origin, %{orig_path_info: conn.path_info, orig_params: conn.params})
      |> redirect(
        to:
          "/?orig_path=#{URI.encode_www_form(conn.request_path)}&orig_query=#{
            URI.encode_www_form(conn.query_string)
          }"
      )
    else
      conn
      # |> delete_session(:original_path)
      # |> delete_session(:original_params)
    end
  end
end
