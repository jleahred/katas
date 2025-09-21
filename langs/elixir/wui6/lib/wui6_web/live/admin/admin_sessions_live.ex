defmodule Wui6Web.AdminSessionsLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts
  alias Wui6.Accounts.Session
  alias Wui6.Accounts.User

  @metadata %{
    title: "Admin · sesiones",
    description: "Listado de sesiones activas ordenadas por último acceso más antiguo.",
    keywords: ["admin", "sessions", "autenticación"]
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:query, "")
     |> assign(:sessions, [])
     |> assign(:page_title, "Sesiones")
     |> assign(:current_path, ~p"/admin/sessions")}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    query =
      params
      |> Map.get("q", "")
      |> to_string()
      |> String.trim()

    sessions = Accounts.list_sessions(query: query)

    {:noreply,
     socket
     |> assign(:sessions, sessions)
     |> assign(:query, query)
     |> assign(:current_path, sessions_path(query))}
  end

  @impl true
  def handle_event("filter", params, socket) do
    trimmed =
      params
      |> Map.get("q", "")
      |> to_string()
      |> String.trim()

    {:noreply, push_patch(socket, to: sessions_path(trimmed))}
  end

  def handle_event("clear_filter", _params, socket) do
    {:noreply, push_patch(socket, to: sessions_path(""))}
  end

  defp sessions_path(""), do: ~p"/admin/sessions"
  defp sessions_path(query), do: ~p"/admin/sessions?#{[q: query]}"

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

  defp format_datetime(nil), do: "—"
  defp format_datetime(other) when is_binary(other), do: other
  defp format_datetime(other), do: to_string(other)

  defp user_email(%Session{user: %User{email: email}}), do: email
  defp user_email(%Session{user: %User{}}), do: "(sin email)"
  defp user_email(_), do: "—"
end
