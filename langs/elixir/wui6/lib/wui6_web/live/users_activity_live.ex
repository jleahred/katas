defmodule Wui6Web.UsersActivityLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts

  @metadata %{
    title: "Admin · actividad de usuarios",
    description:
      "Listado de los 10 usuarios con actividad más antigua o sin actividad registrada.",
    keywords: ["users", "actividad", "admin"]
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Actividad de usuarios")
     |> assign(:current_path, ~p"/users/activity")
     |> load_users()}
  end

  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, load_users(socket)}
  end

  defp load_users(socket) do
    assign(socket, :users, Accounts.list_stale_users(limit: 10))
  end

  defp format_datetime(nil), do: "—"

  defp format_datetime(%NaiveDateTime{} = datetime) do
    datetime
    |> NaiveDateTime.truncate(:second)
    |> Calendar.strftime("%Y-%m-%d %H:%M:%S")
  end

  defp format_datetime(%DateTime{} = datetime) do
    datetime
    |> DateTime.truncate(:second)
    |> Calendar.strftime("%Y-%m-%d %H:%M:%S")
  end

  defp format_datetime(other) when is_binary(other), do: other
  defp format_datetime(other), do: to_string(other)

  defp activity_status(nil), do: "Sin actividad"

  defp activity_status(%NaiveDateTime{} = datetime) do
    "Última actividad: " <> format_datetime(datetime)
  end

  defp activity_status(%DateTime{} = datetime) do
    "Última actividad: " <> format_datetime(datetime)
  end

  defp activity_status(other) do
    "Última actividad: " <> to_string(other)
  end

  defp user_enabled?(user), do: Map.get(user, :enabled, true)

  defp row_text_class(user) do
    if user_enabled?(user), do: "", else: "text-base-content/60"
  end

  defp row_classes(user) do
    base = ["transition-colors"]

    base =
      if user.id == 1 do
        ["bg-error/12" | base]
      else
        base
      end

    Enum.join(base, " ")
  end
end
