defmodule JLEWeb.LogsFileLive do
  use Phoenix.LiveView
  require Logger

  @logs_path "./local/logs"
  @update_millisecs 3000

  def render(assigns) do
    # <pre>
    # <%= inspect(File.stat!("./local/logs/#{assigns.file_name}").mtime, pretty: true)%>
    # </pre>
    # <pre>
    # <%= inspect(assigns, pretty: true)%>
    # </pre>
    ~L"""
      <div class="container">
      <h3>Logs File <code><%=@file_name%></code></h3>
      checked <%= Time.to_string(@date_time) %><p>
      last updated file <%= Time.to_string(@last_updated) %>

        <pre class="bg-light" style="width: 100%; height: 65vh; overflow-x: scroll;">
    <%= for l <- @lines do %><%= l %><% end %>
        </pre>
      </div>
    """
  end

  def mount(session, socket) do
    if connected?(socket), do: :timer.send_interval(@update_millisecs, self(), :tick)
    socket = assign(socket, file_name: session.par["file_name"])
    {:ok, update_file(socket)}
  end

  def handle_info(:tick, socket) do
    {:noreply, update_file(socket)}
  end

  defp update_file(socket) do
    assign(socket,
      lines: get_lines(socket.assigns.file_name),
      date_time: Time.utc_now() |> Time.truncate(:second),
      last_updated: get_lat_updated(socket.assigns.file_name)
    )
  end

  defp get_lat_updated(file_name) do
    File.stat!("#{@logs_path}/#{file_name}").mtime
    |> NaiveDateTime.from_erl!()
  end

  defp get_lines(file_name) do
    insert_list = fn e, l ->
      r = [e | l]

      case length(r) > 600 do
        true -> r |> Enum.take(500)
        false -> r
      end
    end

    "#{@logs_path}/#{file_name}"
    |> File.stream!(line_or_bytes: :line)
    |> Enum.reduce([], &insert_list.(&1, &2))
  end
end
