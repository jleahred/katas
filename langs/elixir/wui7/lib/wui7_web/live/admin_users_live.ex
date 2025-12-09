defmodule Wui7Web.AdminUsersLive do
  use Wui7Web, :live_view

  alias Wui7.Accounts

  def meta_info do
    %{
      title: "Panel de usuarios",
      description: "Listado administrativo de usuarios registrados en la aplicación.",
      keywords: ["admin", "usuarios", "cuentas"]
    }
  end

  @impl true
  def mount(_params, _session, socket) do
    users = Accounts.list_users()

    {:ok,
     socket
     |> assign(:page_title, "Usuarios")
     |> assign(:users, users)
     |> assign(:total_users, length(users))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8">
        <header class="space-y-2">
          <p class="text-sm uppercase tracking-[0.3em] text-base-content/70">Administración</p>
          <div class="flex flex-wrap items-center justify-between gap-4">
            <h1 class="text-3xl font-semibold text-base-content">Usuarios</h1>
            <span class="badge badge-outline text-base-content/70">
              Total: {@total_users}
            </span>
          </div>
          <p class="text-base-content/70">
            Visualiza la actividad básica de cada cuenta registrada.
          </p>
        </header>

        <div class="overflow-hidden rounded-2xl border border-base-300 shadow-sm">
          <table class="min-w-full divide-y divide-base-200">
            <thead class="bg-base-200/60 text-xs font-semibold uppercase tracking-wide text-base-content/70">
              <tr>
                <th class="px-4 py-3 text-left">Usuario</th>
                <th class="px-4 py-3 text-left">Estado</th>
                <th class="px-4 py-3 text-left">Activo</th>
                <th class="px-4 py-3 text-left">Último acceso seguro</th>
                <th class="px-4 py-3 text-left">Creado</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-base-100 text-sm text-base-content/80">
              <tr :for={user <- @users} class="hover:bg-base-200/40 transition">
                <td class="px-4 py-3">
                  <div class="font-semibold text-base-content">{user.email}</div>
                  <div class="text-xs text-base-content/60">ID #{user.id}</div>
                </td>
                <td class="px-4 py-3">
                  <span class={status_badge_classes(user)}>{status_label(user)}</span>
                </td>
                <td class="px-4 py-3">
                  <div class="flex items-center gap-3">
                    <button
                      type="button"
                      id={"toggle-enabled-#{user.id}"}
                      phx-click="toggle-enabled"
                      phx-value-id={user.id}
                      role="switch"
                      aria-checked={user.enabled}
                      class={[
                        "relative inline-flex h-6 w-12 items-center rounded-full border transition-all focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2",
                        user.enabled && "border-success/60 bg-success/90 text-base-100",
                        !user.enabled && "border-base-300 bg-base-200 text-base-content/70"
                      ]}
                    >
                      <span class={[
                        "inline-block h-5 w-5 rounded-full bg-white shadow ring-1 ring-black/5 transition",
                        user.enabled && "translate-x-6",
                        !user.enabled && "translate-x-1"
                      ]} />
                    </button>
                  </div>
                </td>
                <td class="px-4 py-3 font-mono text-xs">
                  {format_datetime(user.authenticated_at) || "—"}
                </td>
                <td class="px-4 py-3 font-mono text-xs">
                  {format_datetime(user.inserted_at)}
                </td>
              </tr>
            </tbody>
          </table>
          <div :if={@users == []} class="px-4 py-6 text-center text-sm text-base-content/70">
            No hay usuarios registrados todavía.
          </div>
        </div>
      </section>
    </Layouts.app>
    """
  end

  @impl true
  def handle_event("toggle-enabled", %{"id" => user_id}, socket) do
    case Accounts.toggle_user_enabled(user_id) do
      {:ok, _user} ->
        {:noreply, assign(socket, :users, Accounts.list_users())}

      {:error, _reason} ->
        {:noreply, socket}
    end
  end

  defp status_label(%{confirmed_at: %DateTime{}}), do: "Confirmado"
  defp status_label(_), do: "Pendiente"

  defp status_badge_classes(%{confirmed_at: %DateTime{}}) do
    "badge badge-success badge-outline"
  end

  defp status_badge_classes(_user), do: "badge badge-warning badge-outline"

  defp format_datetime(nil), do: nil

  defp format_datetime(%DateTime{} = dt) do
    Calendar.strftime(dt, "%Y-%m-%d %H:%M")
  end
end
