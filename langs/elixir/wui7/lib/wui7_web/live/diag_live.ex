defmodule Wui7Web.DiagLive do
  use Wui7Web, :live_view

  def meta_info do
    %{
      title: "Diagnóstico de sesión LiveView",
      description: "Ver parámetros de usuario y conexión en la sesión LiveView actual.",
      keywords: ["diagnóstico", "sesión", "liveview", "usuario", "conexión", "test"]
    }
  end

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Panel de diagnóstico")
      |> assign(:scope_info, scope_info(socket.assigns.current_scope))
      |> assign(:client_info, client_info(socket))

    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-10 animate-fade-in">
        <header class="space-y-3 text-center">
          <p class="text-sm uppercase tracking-[0.3em] text-base-content/70">Diagnóstico</p>
          <h1 class="text-3xl font-semibold text-base-content">Estado de la sesión</h1>
          <p class="text-base-content/70">
            Información útil sobre quién eres y cómo te conectas a esta sesión LiveView.
          </p>
        </header>

        <div class="grid gap-6 lg:grid-cols-2">
          <article class="card border border-base-300 shadow-sm">
            <div class="card-body space-y-4">
              <div>
                <p class="text-xs uppercase text-base-content/60">Identidad detectada</p>
                <h2 class="text-2xl font-semibold text-base-content">Usuario actual</h2>
              </div>
              <dl class="space-y-2 text-sm text-base-content/80">
                <div class="flex items-center justify-between border-b border-base-200/70 pb-2">
                  <dt class="font-medium">Estado</dt>
                  <dd class="text-base-content">{@scope_info.status_label}</dd>
                </div>
                <div class="flex items-center justify-between border-b border-base-200/70 pb-2">
                  <dt class="font-medium">Correo</dt>
                  <dd class="text-base-content">{@scope_info.email || "—"}</dd>
                </div>
                <div class="flex items-center justify-between">
                  <dt class="font-medium">ID interno</dt>
                  <dd class="text-base-content">{@scope_info.user_id || "—"}</dd>
                </div>
              </dl>
            </div>
          </article>

          <article class="card border border-base-300 shadow-sm">
            <div class="card-body space-y-4">
              <div>
                <p class="text-xs uppercase text-base-content/60">Conexión</p>
                <h2 class="text-2xl font-semibold text-base-content">Datos del cliente</h2>
              </div>
              <dl class="space-y-2 text-sm text-base-content/80">
                <div class="flex items-center justify-between border-b border-base-200/70 pb-2">
                  <dt class="font-medium">Estado LiveView</dt>
                  <dd class={[
                    "rounded-full px-3 py-1 text-xs font-semibold",
                    @client_info.connected? && "bg-success/10 text-success",
                    !@client_info.connected? && "bg-warning/10 text-warning"
                  ]}>
                    {if @client_info.connected?, do: "conectado", else: "sin canal"}
                  </dd>
                </div>
                <div class="flex items-center justify-between border-b border-base-200/70 pb-2">
                  <dt class="font-medium">IP remota</dt>
                  <dd class="text-base-content">{@client_info.ip || "—"}</dd>
                </div>
                <div class="flex items-center justify-between border-b border-base-200/70 pb-2">
                  <dt class="font-medium">Puerto</dt>
                  <dd class="text-base-content">{@client_info.port || "—"}</dd>
                </div>
                <div class="flex items-center justify-between border-b border-base-200/70 pb-2">
                  <dt class="font-medium">Forwarded for</dt>
                  <dd class="text-base-content">{@client_info.forwarded_for || "—"}</dd>
                </div>
                <div>
                  <dt class="font-medium">User Agent</dt>
                  <dd class="mt-1 rounded-lg bg-base-200/60 p-3 text-xs font-mono leading-relaxed">
                    {@client_info.user_agent || "No disponible"}
                  </dd>
                </div>
              </dl>
            </div>
          </article>
        </div>
      </section>
    </Layouts.app>
    """
  end

  defp scope_info(nil) do
    %{
      status_label: "Invitado",
      email: nil,
      user_id: nil
    }
  end

  defp scope_info(%{user: nil}), do: scope_info(nil)

  defp scope_info(%{user: user}) do
    %{
      status_label: "Usuario autenticado",
      email: user.email,
      user_id: user.id
    }
  end

  defp client_info(socket) do
    peer = get_connect_data(socket, :peer_data) || %{}
    user_agent = get_connect_data(socket, :user_agent)
    headers = get_connect_data(socket, :x_headers) || []

    %{
      connected?: connected?(socket),
      ip: format_ip(peer[:address]),
      port: peer[:port],
      user_agent: user_agent,
      forwarded_for: forwarded_for(headers)
    }
  end

  defp format_ip(nil), do: nil
  defp format_ip({_, _, _, _} = tuple), do: tuple |> :inet.ntoa() |> List.to_string()
  defp format_ip({_, _, _, _, _, _, _, _} = tuple), do: tuple |> :inet.ntoa() |> List.to_string()
  defp format_ip(other) when is_binary(other), do: other
  defp format_ip(_), do: nil

  defp forwarded_for(nil), do: nil

  defp forwarded_for(headers) do
    headers
    |> Enum.find_value(fn
      {"x-forwarded-for", value} -> value
      {"x-forwarded-for", value, _} -> value
      _ -> nil
    end)
  end

  defp get_connect_data(socket, key) do
    if connected?(socket) do
      Phoenix.LiveView.get_connect_info(socket, key)
    end
  end
end
