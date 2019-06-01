defmodule JLEWeb.LogsLive do
  use Phoenix.LiveView
  require Logger

  @logs_path "./local/logs"
  @update_millisecs 8000

  def render(assigns) do
    ~L"""

    <div class="container">
      <h3>Logs</h3>

      <%= Time.to_string(@date_time) %>
      <p>

      <p>

      <div class="row">
        <%= for ff <- @files_folders do %>
        <div class="p-4">
          <h4> <%= ff.folder %> </h4>
          <table class="table">
            <%= for f <- ff.files do %>
              <tr>
              <td> <a href=<%= "/logs/file?file_name=#{ff.folder}/#{f.name}" %>> <%= f.name %> </a> </td>
              <td> <%= f.date_time %> </td>
              <td><i phx-click="del-<%="#{ff.folder}/#{f.name}"%>" class="far fa-trash-alt"></i></td>
              </tr>
            <% end %>
          </table>
        </div>
          <% end %>
      </div>
    </div>
    """
  end

  def mount(_session, socket) do
    if connected?(socket), do: :timer.send_interval(@update_millisecs, self(), :tick)

    {:ok, update_files(socket)}
  end

  def handle_event("del-" <> file, _, socket) do
    File.rm!("#{@logs_path}/#{file}")
    {:noreply, update_files(socket)}
  end

  # defp get_logs_fake() do
  #   [
  #     %{folder: ".", files: [%{name: "aab", date_time: NaiveDateTime.utc_now()}]},
  #     %{folder: "pr", files: []}
  #   ]
  # end

  defp get_logs() do
    get_file_and_folders_rec(".", [])
    |> Enum.sort_by(fn ff -> ff.folder end, &<=/2)
  end

  defp add_file_folder(full_path, f, acc) do
    file_path = "#{full_path}/#{f}"

    case File.dir?(file_path) do
      true ->
        %{folders: [f | acc.folders], files: acc.files}

      false ->
        file_date =
          File.stat!(file_path).mtime
          |> NaiveDateTime.from_erl!()

        %{
          folders: acc.folders,
          files: [%{name: f, date_time: file_date} | acc.files]
        }
    end
  end

  defp get_file_and_folders_rec(path, acc) do
    full_path = "#{@logs_path}/#{path}"

    ff =
      full_path
      |> File.ls!()
      |> Enum.reduce(%{folders: [], files: []}, &add_file_folder(full_path, &1, &2))

    current = %{
      files: ff.files |> Enum.sort_by(fn bi -> bi.date_time end, &>=/2),
      folder: path
    }

    rec =
      case ff.folders do
        [] ->
          acc

        folder ->
          folder
          |> Enum.reduce([], fn f, acc ->
            get_file_and_folders_rec("#{path}/#{f}", acc) ++ acc
          end)
      end

    [current | rec]
  end

  def handle_info(:tick, socket) do
    {:noreply, update_files(socket)}
  end

  defp update_files(socket) do
    assign(socket,
      files_folders: get_logs(),
      date_time: Time.utc_now() |> Time.truncate(:second)
    )
  end
end
