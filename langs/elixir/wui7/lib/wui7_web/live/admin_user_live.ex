defmodule Wui7Web.AdminUserLive do
  use Wui7Web, :live_view

  alias Wui7.Accounts

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Detalle de usuario")
     |> assign(:user, nil)
     |> assign(:tokens, [])}
  end

  @impl true
  def handle_params(%{"id" => id}, _uri, socket) do
    case Accounts.get_user(id) do
      %Accounts.User{} = user ->
        tokens = Accounts.list_user_tokens(user)

        {:noreply,
         socket
         |> assign(:user, user)
         |> assign(:tokens, tokens)}

      _ ->
        {:noreply,
         socket
         |> put_flash(:error, "Usuario no encontrado")
         |> push_navigate(to: ~p"/admin/users")}
    end
  end

  @impl true
  def render(%{user: nil} = assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8">
        <div class="rounded-2xl border border-base-300 bg-base-100 p-10 text-center text-base-content/70">
          Cargando usuario...
        </div>
      </section>
    </Layouts.app>
    """
  end

  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8">
        <header class="flex flex-wrap items-center justify-between gap-4">
          <div>
            <p class="text-sm uppercase tracking-[0.3em] text-base-content/60">Administración</p>
            <h1 class="text-3xl font-semibold text-base-content">{@user.email}</h1>
            <p class="text-base-content/70">Detalles completos de la cuenta.</p>
          </div>
          <.link navigate={~p"/admin/users"} class="btn btn-outline btn-sm">
            &larr; Volver a usuarios
          </.link>
        </header>

        <div class="grid gap-6 lg:grid-cols-2">
          <div class="rounded-2xl border border-base-300 bg-base-100 p-6 shadow-sm space-y-4">
            <h2 class="text-lg font-semibold text-base-content">Información general</h2>
            <dl class="space-y-3 text-sm text-base-content/80">
              <div class="flex items-center justify-between">
                <dt class="text-base-content/60">Correo</dt>
                <dd class="font-medium text-base-content">{@user.email}</dd>
              </div>
              <div class="flex items-center justify-between">
                <dt class="text-base-content/60">ID</dt>
                <dd class="font-mono text-base-content">{@user.id}</dd>
              </div>
              <div class="flex items-center justify-between">
                <dt class="text-base-content/60">Estado</dt>
                <dd>
                  <span class={status_badge_classes(@user)}>{status_label(@user)}</span>
                </dd>
              </div>
              <div class="flex items-center justify-between">
                <dt class="text-base-content/60">Activo</dt>
                <dd>
                  <span class={enabled_badge_classes(@user.enabled)}>
                    {if(@user.enabled, do: "Habilitado", else: "Deshabilitado")}
                  </span>
                </dd>
              </div>
            </dl>
          </div>

          <div class="rounded-2xl border border-base-300 bg-base-100 p-6 shadow-sm space-y-3 text-sm text-base-content/80">
            <h2 class="text-lg font-semibold text-base-content">Fechas clave</h2>
            <div class="flex items-center justify-between">
              <span class="text-base-content/60">Creación</span>
              <span class="font-mono">{format_datetime(@user.inserted_at) || "—"}</span>
            </div>
            <div class="flex items-center justify-between">
              <span class="text-base-content/60">Última actualización</span>
              <span class="font-mono">{format_datetime(@user.updated_at) || "—"}</span>
            </div>
            <div class="flex items-center justify-between">
              <span class="text-base-content/60">Confirmación</span>
              <span class="font-mono">{format_datetime(@user.confirmed_at) || "—"}</span>
            </div>
            <div class="flex items-center justify-between">
              <span class="text-base-content/60">Último acceso seguro</span>
              <span class="font-mono">{format_datetime(@user.authenticated_at) || "—"}</span>
            </div>
          </div>
        </div>

        <section class="rounded-2xl border border-base-300 bg-base-100 shadow-sm">
          <header class="flex items-center justify-between border-b border-base-200 px-6 py-4">
            <div>
              <h3 class="text-lg font-semibold text-base-content">Tokens activos</h3>
              <p class="text-sm text-base-content/70">
                Registros vinculados a sesiones, enlaces mágicos o cambios de correo.
              </p>
            </div>
            <span class="badge badge-outline text-base-content/70">Total: {length(@tokens)}</span>
          </header>
          <div class="overflow-x-auto">
            <table class="min-w-full divide-y divide-base-200 text-sm">
              <thead class="bg-base-200/60 text-xs uppercase tracking-wide text-base-content/70">
                <tr>
                  <th class="px-4 py-3 text-left">Token</th>
                  <th class="px-4 py-3 text-left">Contexto</th>
                  <th class="px-4 py-3 text-left">Destinatario</th>
                  <th class="px-4 py-3 text-left">Creado</th>
                  <th class="px-4 py-3 text-left">Último login</th>
                </tr>
              </thead>
              <tbody class="divide-y divide-base-100 text-base-content/80">
                <tr :for={token <- @tokens} class="hover:bg-base-200/30 transition">
                  <td class="px-4 py-3 font-mono text-xs">
                    <span class="break-all">{format_token(token)}</span>
                  </td>
                  <td class="px-4 py-3">
                    <span class="badge badge-outline">{token.context}</span>
                  </td>
                  <td class="px-4 py-3 text-xs">
                    {token.sent_to || "—"}
                  </td>
                  <td class="px-4 py-3 font-mono text-xs">
                    {format_datetime(token.inserted_at) || "—"}
                  </td>
                  <td class="px-4 py-3 font-mono text-xs">
                    {format_datetime(token.authenticated_at) || "—"}
                  </td>
                </tr>
              </tbody>
            </table>
            <div :if={@tokens == []} class="px-6 py-8 text-center text-sm text-base-content/70">
              No hay tokens registrados para este usuario.
            </div>
          </div>
        </section>
      </section>
    </Layouts.app>
    """
  end

  defp status_label(%{confirmed_at: %DateTime{}}), do: "Confirmado"
  defp status_label(_), do: "Pendiente"

  defp status_badge_classes(%{confirmed_at: %DateTime{}}) do
    "badge badge-success badge-outline"
  end

  defp status_badge_classes(_user), do: "badge badge-warning badge-outline"

  defp enabled_badge_classes(true), do: "badge badge-success badge-outline"
  defp enabled_badge_classes(false), do: "badge badge-neutral badge-outline"

  defp format_datetime(nil), do: nil

  defp format_datetime(%DateTime{} = dt) do
    Calendar.strftime(dt, "%Y-%m-%d %H:%M")
  end

  defp format_token(%{token: token}) when is_binary(token) do
    Base.encode64(token)
  end

  defp format_token(_), do: "—"
end
