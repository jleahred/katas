defmodule Wui7Web.AdminRolesLive do
  use Wui7Web, :live_view

  alias Wui7.Accounts
  alias Wui7.Accounts.Role

  def meta_info do
    %{
      title: "Roles",
      description: "Gestión de roles de acceso.",
      keywords: ["roles", "admin", "seguridad"]
    }
  end

  @impl true
  def mount(_params, _session, socket) do
    roles = Accounts.list_roles()

    {:ok,
     socket
     |> assign(:page_title, "Roles")
     |> assign(:roles, roles)
     |> assign(:modal_open?, false)
     |> assign(:editing_role, nil)
     |> assign_form(Role.changeset(%Role{}, %{}))}
  end

  @impl true
  def handle_event("new-role", _params, socket) do
    {:noreply,
     socket
     |> assign(:modal_open?, true)
     |> assign(:editing_role, nil)
     |> assign_form(Role.changeset(%Role{}, %{}))}
  end

  def handle_event("edit-role", %{"id" => id}, socket) do
    role = Accounts.get_role!(id)

    {:noreply,
     socket
     |> assign(:modal_open?, true)
     |> assign(:editing_role, role)
     |> assign_form(Accounts.change_role(role))}
  end

  def handle_event("cancel-edit", _params, socket) do
    {:noreply,
     socket
     |> assign(:modal_open?, false)
     |> assign(:editing_role, nil)
     |> assign_form(Accounts.change_role(%Role{}))}
  end

  def handle_event("close-modal", _params, socket) do
    {:noreply, assign(socket, :modal_open?, false)}
  end

  def handle_event("save-role", %{"role" => params}, socket) do
    save_role(socket, socket.assigns.editing_role, params)
  end

  def handle_event("delete-role", %{"id" => id}, socket) do
    role = Accounts.get_role!(id)
    {:ok, _} = Accounts.delete_role(role)

    {:noreply,
     socket
     |> put_flash(:info, "Rol eliminado")
     |> refresh_roles()}
  end

  defp save_role(socket, nil, params) do
    case Accounts.create_role(params) do
      {:ok, _role} ->
        {:noreply,
         socket
         |> put_flash(:info, "Rol creado")
         |> assign(:modal_open?, false)
         |> assign(:editing_role, nil)
         |> refresh_roles()
         |> assign_form(Accounts.change_role(%Role{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp save_role(socket, role, params) do
    case Accounts.update_role(role, params) do
      {:ok, role} ->
        {:noreply,
         socket
         |> put_flash(:info, "Rol actualizado")
         |> assign(:modal_open?, false)
         |> assign(:editing_role, role)
         |> refresh_roles()
         |> assign_form(Accounts.change_role(role))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp refresh_roles(socket) do
    assign(socket, :roles, Accounts.list_roles())
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, :form, to_form(changeset))
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8">
        <header class="flex flex-wrap items-center justify-between gap-4">
          <div>
            <p class="text-sm uppercase tracking-[0.3em] text-base-content/60">Administración</p>
            <h1 class="text-3xl font-semibold text-base-content">Roles</h1>
            <p class="text-base-content/70">
              Gestiona los códigos de rol disponibles en la plataforma.
            </p>
          </div>
          <button class="btn btn-primary btn-sm" phx-click="new-role">
            <.icon name="hero-plus" class="w-4 h-4" /> Nuevo rol
          </button>
        </header>

        <div class="rounded-2xl border border-base-300 bg-base-100 shadow-sm">
          <div class="border-b border-base-200 px-6 py-4">
            <h2 class="text-lg font-semibold text-base-content">Listado ({length(@roles)})</h2>
          </div>
          <div class="overflow-x-auto">
            <table class="min-w-full divide-y divide-base-200 text-sm">
              <thead class="bg-base-200/60 text-xs uppercase tracking-wide text-base-content/70">
                <tr>
                  <th class="px-4 py-3 text-left">Código</th>
                  <th class="px-4 py-3 text-left">Descripción</th>
                  <th class="px-4 py-3 text-right">Acciones</th>
                </tr>
              </thead>
              <tbody class="divide-y divide-base-100 text-base-content/80">
                <tr :for={role <- @roles} class="hover:bg-base-200/30 transition">
                  <td class="px-4 py-3 font-mono text-xs lowercase">{role.code}</td>
                  <td class="px-4 py-3">{role.description}</td>
                  <td class="px-4 py-3 flex justify-end gap-2">
                    <button
                      class="btn btn-ghost btn-xs"
                      phx-click="edit-role"
                      phx-value-id={role.id}
                    >
                      <.icon name="hero-pencil-square" class="w-4 h-4" />
                    </button>
                    <button
                      class="btn btn-ghost btn-xs text-error"
                      phx-click="delete-role"
                      data-confirm="¿Eliminar rol?"
                      phx-value-id={role.id}
                    >
                      <.icon name="hero-trash" class="w-4 h-4" />
                    </button>
                  </td>
                </tr>
                <tr :if={@roles == []}>
                  <td colspan="3" class="px-4 py-6 text-center text-base-content/60">
                    No hay roles registrados todavía.
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>

        <div
          id="role-modal"
          class={[
            "modal",
            @modal_open? && "modal-open"
          ]}
        >
          <div class="modal-box max-w-lg space-y-6">
            <div class="flex items-center justify-between">
              <div>
                <p class="text-sm uppercase tracking-[0.3em] text-base-content/60">
                  {if @editing_role, do: "Editar rol", else: "Nuevo rol"}
                </p>
                <h2 class="text-2xl font-semibold text-base-content">
                  {if @editing_role,
                    do: "Actualiza el rol #{@editing_role.code}",
                    else: "Crea un nuevo rol"}
                </h2>
              </div>
              <button class="btn btn-ghost btn-sm" phx-click="close-modal" aria-label="Cerrar">
                <.icon name="hero-x-mark" class="w-5 h-5" />
              </button>
            </div>

            <div
              :if={@editing_role}
              class="rounded-2xl border border-base-300 bg-base-200/40 p-4 text-sm text-base-content/80"
            >
              <dl class="space-y-2">
                <div class="flex items-center justify-between">
                  <dt class="text-base-content/60">ID</dt>
                  <dd class="font-mono text-base-content">{@editing_role.id}</dd>
                </div>
                <div class="flex items-center justify-between">
                  <dt class="text-base-content/60">Creado</dt>
                  <dd class="font-mono">{format_datetime(@editing_role.inserted_at) || "—"}</dd>
                </div>
                <div class="flex items-center justify-between">
                  <dt class="text-base-content/60">Actualizado</dt>
                  <dd class="font-mono">{format_datetime(@editing_role.updated_at) || "—"}</dd>
                </div>
              </dl>
            </div>

            <.form for={@form} id="role-form" phx-submit="save-role" class="space-y-4">
              <.input
                field={@form[:code]}
                label="Código"
                placeholder="admin"
                class="lowercase tracking-wide font-mono"
              />
              <.input
                field={@form[:description]}
                label="Descripción"
                type="text"
                placeholder="Rol con acceso completo"
              />
              <div class="flex justify-end gap-2">
                <button type="button" class="btn btn-ghost" phx-click="cancel-edit">
                  Cancelar
                </button>
                <button type="submit" class="btn btn-primary">
                  {if @editing_role, do: "Guardar cambios", else: "Crear rol"}
                </button>
              </div>
            </.form>
          </div>
          <div class="modal-backdrop bg-black/40" phx-click="close-modal" />
        </div>
      </section>
    </Layouts.app>
    """
  end

  defp format_datetime(nil), do: nil

  defp format_datetime(%DateTime{} = dt) do
    Calendar.strftime(dt, "%Y-%m-%d %H:%M")
  end
end
