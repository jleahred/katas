defmodule Wui6Web.UserLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts
  alias Wui6.Accounts.Session
  alias Wui6.Accounts.User

  @metadata %{
    title: "Mi cuenta",
    description: "Resumen de tokens de acceso activos para el usuario autenticado.",
    keywords: ["usuario", "tokens", "sesiones"]
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    case socket.assigns[:current_session_user] do
      %User{id: user_id} = user ->
        current_session = socket.assigns[:current_session]
        sessions = Accounts.list_sessions_for_user(user_id)

        current_token =
          socket.assigns[:register_token] ||
            case current_session do
              %Session{token: token} -> token
              _ -> nil
            end

        {:ok,
         socket
         |> assign(:user, user)
         |> assign(:current_token, current_token)
         |> assign(:sessions, sessions)
         |> assign(:page_title, @metadata.title)
         |> assign(:current_path, ~p"/user")}

      _ ->
        {:ok,
         socket
         |> put_flash(:error, "Necesitas registrar una sesión para acceder a tu perfil.")
         |> redirect(to: ~p"/admin/user/register")}
    end
  end

  @impl true
  def handle_info({:session_refreshed, token}, socket) do
    {:noreply, refresh_sessions(socket, token)}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  defp refresh_sessions(socket, token) do
    case socket.assigns[:user] do
      %User{id: user_id} = user ->
        sessions = Accounts.list_sessions_for_user(user_id)

        socket
        |> assign(:sessions, sessions)
        |> assign(:current_token, token || socket.assigns[:current_token])
        |> assign(:user, user)

      _ ->
        socket
    end
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

  defp session_row_class(%Session{token: token}, current_token) do
    if token == current_token, do: "bg-primary/10", else: ""
  end

  defp decode_context(nil), do: %{}

  defp decode_context(value) when is_binary(value) do
    case Jason.decode(value) do
      {:ok, map} -> map
      _ -> %{}
    end
  end

  defp decode_context(_), do: %{}

  defp alerts_for(session) do
    session
    |> Map.get(:context_info)
    |> decode_context()
    |> Map.get("alerts", [])
  end

  defp last_seen_info(session) do
    session
    |> Map.get(:context_info)
    |> decode_context()
    |> Map.get("last_seen", %{})
  end

  defp initial_info(session) do
    session
    |> Map.get(:context_info)
    |> decode_context()
    |> Map.get("initial", %{})
  end

  defp map_to_lines(map) when is_map(map) do
    map
    |> Enum.reject(fn {_k, v} -> is_nil(v) or v == "" end)
    |> Enum.map(fn {key, value} ->
      "#{key}: #{value}"
    end)
  end

  defp map_to_lines(_), do: []

  attr :session, Session, required: true
  attr :current_token, :string, required: true

  def session_details(assigns) do
    alerts = alerts_for(assigns.session)
    last_seen = map_to_lines(last_seen_info(assigns.session))
    initial = map_to_lines(initial_info(assigns.session))

    assigns =
      assigns
      |> assign(:alerts, alerts)
      |> assign(:last_seen, last_seen)
      |> assign(:initial, initial)

    ~H"""
    <tr class={session_row_class(@session, @current_token)}>
      <td class="font-mono text-xs break-all">
        {@session.token}
        <span :if={@session.token == @current_token} class="badge badge-primary ml-2">Actual</span>
      </td>
      <td>{format_datetime(@session.created_at)}</td>
      <td>{format_datetime(@session.last_access)}</td>
      <td>
        <ul class="text-xs text-base-content/70 space-y-1">
          <li :for={line <- @last_seen}>{line}</li>
        </ul>
      </td>
      <td>
        <ul class="text-xs text-base-content/60 space-y-1">
          <li :for={line <- @initial}>{line}</li>
        </ul>
      </td>
      <td>
        <ul class="text-xs text-warning space-y-1">
          <li :for={alert <- @alerts}>
            <span class="font-semibold uppercase">{alert["type"]}</span>
            <span class="block">Esperado: {alert["expected"]}</span>
            <span class="block">Detectado: {alert["observed"]}</span>
            <span class="block text-base-content/50">En: {alert["at"]}</span>
          </li>
          <li :if={@alerts == []} class="text-base-content/50">Sin alertas</li>
        </ul>
      </td>
    </tr>
    """
  end
end
