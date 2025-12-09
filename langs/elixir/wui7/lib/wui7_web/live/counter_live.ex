defmodule Wui7Web.CounterLive do
  use Wui7Web, :live_view

  # codex

  @count_param "count"
  @default_count 0
  @min_count -999
  @max_count 999

  def meta_info do
    %{
      title: "Contador sincronizado con URL",
      description:
        "Incrementa, decrementa o reinicia y comparte el resultado mediante el parámetro count.",
      keywords: ["contador", "url", "liveview", "formularios", "compartir"]
    }
  end

  @impl true
  def mount(params, _session, socket) do
    counter = read_counter(params)

    {:ok,
     socket
     |> assign(:page_title, "Contador reactivo")
     |> assign(:share_url, "")
     |> assign(:min_count, @min_count)
     |> assign(:max_count, @max_count)
     |> assign_counter(counter)}
  end

  @impl true
  def handle_params(params, uri, socket) do
    {:noreply,
     socket
     |> assign(:share_url, uri)
     |> assign_counter(read_counter(params))}
  end

  @impl true
  def handle_event("increment", _params, socket) do
    {:noreply, push_count(socket, socket.assigns.counter + 1)}
  end

  def handle_event("decrement", _params, socket) do
    {:noreply, push_count(socket, socket.assigns.counter - 1)}
  end

  def handle_event("reset", _params, socket) do
    {:noreply, push_count(socket, @default_count)}
  end

  def handle_event("set_from_form", %{"counter" => counter_params}, socket) do
    {:noreply, push_count(socket, parse_count(counter_params))}
  end

  defp push_count(socket, counter) do
    push_patch(socket, to: counter_path(counter))
  end

  defp assign_counter(socket, counter) do
    socket
    |> assign(:counter, counter)
    |> assign(:form, to_form(%{"count" => Integer.to_string(counter)}, as: :counter))
  end

  defp read_counter(params), do: parse_count(params)

  defp parse_count(params) do
    params
    |> Map.get(@count_param)
    |> normalize_count()
  end

  defp normalize_count(nil), do: @default_count

  defp normalize_count(value) when is_binary(value) do
    value = String.trim(value)

    case Integer.parse(value) do
      {int, _} -> clamp(int)
      :error -> @default_count
    end
  end

  defp clamp(value) when value < @min_count, do: @min_count
  defp clamp(value) when value > @max_count, do: @max_count
  defp clamp(value), do: value

  defp counter_path(counter), do: ~p"/counter?count=#{counter}"

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8 animate-fade-in">
        <article class="card bg-base-100 shadow-xl border border-base-300/60">
          <div class="card-body gap-6">
            <div class="space-y-3">
              <p class="badge badge-primary badge-outline px-4 py-3 text-xs uppercase tracking-[0.3em]">
                Contador reactivo
              </p>
              <h1 class="text-3xl sm:text-4xl font-semibold tracking-tight">
                Comparte el estado del contador directamente en la URL
              </h1>
              <p class="text-base-content/70 leading-relaxed">
                Cada acción actualiza el parámetro
                <code class="badge badge-neutral font-mono">count</code>
                en la barra de direcciones.
                Copia y pega el enlace para retomar exactamente el mismo valor o edita el número desde la barra de direcciones.
              </p>
            </div>

            <div class="space-y-6">
              <div class="stat glass text-center hover:shadow-2xl transition-all duration-300 rounded-2xl border border-base-200/80">
                <div class="stat-title text-base-content/70 uppercase tracking-widest text-xs">
                  Valor actual
                </div>
                <div
                  id="counter-value"
                  class="stat-value text-5xl sm:text-6xl font-black text-primary"
                >
                  {@counter}
                </div>
                <div class="stat-desc text-base-content/60">Sincronizado con la URL</div>
              </div>

              <div class="space-y-4">
                <div class="flex justify-center">
                  <div class="btn-group rounded-2xl shadow-lg">
                    <button
                      id="decrement-btn"
                      class="btn btn-outline btn-secondary px-6 text-lg font-semibold transition hover:translate-y-0.5"
                      phx-click="decrement"
                      phx-disable-with="..."
                    >
                      -1
                    </button>
                    <button
                      id="reset-btn"
                      class="btn btn-outline px-6 text-lg font-semibold transition hover:translate-y-0.5"
                      phx-click="reset"
                      phx-disable-with="..."
                    >
                      Reiniciar
                    </button>
                    <button
                      id="increment-btn"
                      class="btn btn-primary px-6 text-lg font-semibold transition hover:-translate-y-0.5"
                      phx-click="increment"
                      phx-disable-with="..."
                    >
                      +1
                    </button>
                  </div>
                </div>
                <div class="rounded-2xl bg-base-200/70 border border-base-300/70 px-4 py-3">
                  <p class="text-xs uppercase tracking-[0.35em] text-base-content/50 mb-1">
                    Enlace con estado
                  </p>
                  <div class="flex items-center gap-2">
                    <.icon name="hero-link" class="size-4 text-primary" />
                    <p class="text-sm font-mono text-base-content/80 truncate" title={@share_url}>
                      {@share_url}
                    </p>
                  </div>
                </div>
              </div>

              <div class="rounded-2xl bg-base-200/60 border border-base-300/80 p-5">
                <p class="text-sm text-base-content/70 mb-3">Actualizar manualmente:</p>
                <.form
                  for={@form}
                  id="counter-form"
                  class="flex items-end gap-4"
                  phx-submit="set_from_form"
                >
                  <.input
                    field={@form[:count]}
                    type="number"
                    min={@min_count}
                    max={@max_count}
                    step="1"
                    inputmode="numeric"
                    class="input input-bordered w-full font-mono text-lg"
                    placeholder="0"
                  />
                  <button
                    type="submit"
                    class="btn btn-primary px-8 transition-transform hover:scale-[1.02]"
                  >
                    Aplicar
                  </button>
                </.form>
                <p class="text-xs text-base-content/60 mt-3">
                  Rango permitido: {@min_count} a {@max_count}
                </p>
              </div>
            </div>
          </div>
        </article>
      </section>
    </Layouts.app>
    """
  end
end
