defmodule Wui7Web.Counter2Live do
  use Wui7Web, :live_view

  @count_a_param "count_a"
  @count_b_param "count_b"
  @default_count 0
  @min_count -999
  @max_count 999

  @impl true
  def mount(params, _session, socket) do
    {count_a, count_b} = read_pair(params)

    {:ok,
     socket
     |> assign(:current_scope, nil)
     |> assign(:page_title, "Contador doble")
     |> assign(:share_url, "")
     |> assign(:min_count, @min_count)
     |> assign(:max_count, @max_count)
     |> assign_pair(count_a, count_b)}
  end

  @impl true
  def handle_params(params, uri, socket) do
    {count_a, count_b} = read_pair(params)

    {:noreply,
     socket
     |> assign(:share_url, uri)
     |> assign_pair(count_a, count_b)}
  end

  @impl true
  def handle_event("adjust", %{"counter" => counter, "delta" => delta}, socket) do
    delta = parse_count(delta)

    {count_a, count_b} =
      case counter do
        "a" -> {socket.assigns.count_a + delta, socket.assigns.count_b}
        "b" -> {socket.assigns.count_a, socket.assigns.count_b + delta}
        _ -> {socket.assigns.count_a, socket.assigns.count_b}
      end

    {:noreply, push_pair(socket, count_a, count_b)}
  end

  def handle_event("reset", %{"counter" => counter}, socket) do
    {count_a, count_b} =
      case counter do
        "a" -> {@default_count, socket.assigns.count_b}
        "b" -> {socket.assigns.count_a, @default_count}
        _ -> {@default_count, @default_count}
      end

    {:noreply, push_pair(socket, count_a, count_b)}
  end

  def handle_event("sync_pair", %{"pair" => pair_params}, socket) do
    count_a = pair_params[@count_a_param] |> parse_count()
    count_b = pair_params[@count_b_param] |> parse_count()

    {:noreply, push_pair(socket, count_a, count_b)}
  end

  defp push_pair(socket, count_a, count_b) do
    count_a = clamp(count_a)
    count_b = clamp(count_b)
    push_patch(socket, to: counter2_path(count_a, count_b))
  end

  defp assign_pair(socket, count_a, count_b) do
    count_a = clamp(count_a)
    count_b = clamp(count_b)

    socket
    |> assign(:count_a, count_a)
    |> assign(:count_b, count_b)
    |> assign(:total, count_a + count_b)
    |> assign(:gap, abs(count_a - count_b))
    |> assign(:leader, leader_text(count_a, count_b))
    |> assign(:status_a, counter_status(:a, count_a, count_b))
    |> assign(:status_b, counter_status(:b, count_a, count_b))
    |> assign(:pair_form, pair_form(count_a, count_b))
  end

  defp leader_text(count_a, count_b) do
    cond do
      count_a > count_b -> "A va en cabeza"
      count_b > count_a -> "B va en cabeza"
      true -> "Empate perfecto"
    end
  end

  defp counter_status(:a, count_a, count_b) do
    cond do
      count_a > count_b -> "Líder"
      count_a < count_b -> "Rezago"
      true -> "Empate"
    end
  end

  defp counter_status(:b, count_a, count_b) do
    cond do
      count_b > count_a -> "Líder"
      count_b < count_a -> "Rezago"
      true -> "Empate"
    end
  end

  defp pair_form(count_a, count_b) do
    to_form(
      %{
        @count_a_param => Integer.to_string(count_a),
        @count_b_param => Integer.to_string(count_b)
      },
      as: :pair
    )
  end

  defp read_pair(params) do
    {
      parse_count(params[@count_a_param]),
      parse_count(params[@count_b_param])
    }
  end

  defp parse_count(nil), do: @default_count

  defp parse_count(value) when is_binary(value) do
    value = String.trim(value)

    case Integer.parse(value) do
      {int, _} -> int
      :error -> @default_count
    end
  end

  defp clamp(value) when value < @min_count, do: @min_count
  defp clamp(value) when value > @max_count, do: @max_count
  defp clamp(value), do: value

  defp counter2_path(count_a, count_b), do: ~p"/counter2?count_a=#{count_a}&count_b=#{count_b}"

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8 animate-fade-in">
        <article class="card bg-base-100 shadow-xl border border-base-300/60">
          <div class="card-body space-y-8">
            <header class="space-y-3">
              <p class="badge badge-primary badge-outline px-4 py-3 text-xs uppercase tracking-[0.3em]">
                Contador doble
              </p>
              <h1 class="text-3xl sm:text-4xl font-semibold tracking-tight">
                Coordina dos contadores sincronizados con la URL
              </h1>
              <p class="text-base-content/70 leading-relaxed">
                Controla dos valores independientes que se comparten mediante los parámetros
                <code class="badge badge-neutral font-mono">count_a</code>
                y <code class="badge badge-neutral font-mono">count_b</code>.
              </p>
            </header>

            <div class="grid gap-6 lg:grid-cols-2">
              <.card
                id="counter-a"
                label="Contador A"
                value={@count_a}
                status={@status_a}
                color="primary"
                counter="a"
              />
              <.card
                id="counter-b"
                label="Contador B"
                value={@count_b}
                status={@status_b}
                color="secondary"
                counter="b"
              />
            </div>

            <div class="grid gap-4 md:grid-cols-3">
              <div class="stat bg-base-200/60 rounded-2xl">
                <div class="stat-title">Suma total</div>
                <div class="stat-value text-primary">{@total}</div>
                <div class="stat-desc">Resultado de A + B</div>
              </div>
              <div class="stat bg-base-200/60 rounded-2xl">
                <div class="stat-title">Diferencia</div>
                <div class="stat-value text-secondary">{@gap}</div>
                <div class="stat-desc">Distancia absoluta</div>
              </div>
              <div class="stat bg-base-200/60 rounded-2xl">
                <div class="stat-title">Situación</div>
                <div class="stat-value text-accent text-lg">{@leader}</div>
                <div class="stat-desc">Actualizada en tiempo real</div>
              </div>
            </div>

            <div class="rounded-2xl bg-base-200/70 border border-base-300/70 p-5 space-y-4">
              <p class="text-sm text-base-content/70">Actualizar ambos manualmente:</p>
              <.form
                for={@pair_form}
                id="pair-form"
                class="grid gap-4 md:grid-cols-[1fr_auto_1fr_auto] items-end"
                phx-submit="sync_pair"
              >
                <.input
                  field={@pair_form[:count_a]}
                  type="number"
                  min={@min_count}
                  max={@max_count}
                  step="1"
                  inputmode="numeric"
                  class="input input-bordered font-mono text-lg"
                  placeholder="0"
                  label="Count A"
                />
                <span class="text-center text-sm text-base-content/60">y</span>
                <.input
                  field={@pair_form[:count_b]}
                  type="number"
                  min={@min_count}
                  max={@max_count}
                  step="1"
                  inputmode="numeric"
                  class="input input-bordered font-mono text-lg"
                  placeholder="0"
                  label="Count B"
                />
                <button type="submit" class="btn btn-primary h-12 px-10">Aplicar</button>
              </.form>
              <p class="text-xs text-base-content/60">Rango permitido: {@min_count} a {@max_count}</p>
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
        </article>
      </section>
    </Layouts.app>
    """
  end

  attr :id, :string, required: true
  attr :label, :string, required: true
  attr :value, :integer, required: true
  attr :status, :string, required: true
  attr :color, :string, required: true
  attr :counter, :string, required: true

  def card(assigns) do
    ~H"""
    <div class="card border border-base-300/70 shadow-lg">
      <div class="card-body space-y-4">
        <div class="flex items-center justify-between">
          <p class="text-sm uppercase tracking-[0.3em] text-base-content/60">{@label}</p>
          <span class={["badge badge-outline", "badge-#{@color}"]}>{@status}</span>
        </div>
        <p id={"#{@id}-value"} class={["text-5xl font-black", "text-#{@color}"]}>{@value}</p>
        <div class="flex flex-wrap gap-3 justify-center">
          <button
            type="button"
            class="btn btn-outline btn-sm"
            phx-click="adjust"
            phx-value-counter={@counter}
            phx-value-delta="-5"
            id={"#{@id}-dec-5"}
          >
            -5
          </button>
          <button
            type="button"
            class="btn btn-outline btn-sm"
            phx-click="adjust"
            phx-value-counter={@counter}
            phx-value-delta="-1"
            id={"#{@id}-dec-1"}
          >
            -1
          </button>
          <button
            type="button"
            class="btn btn-outline btn-sm"
            phx-click="adjust"
            phx-value-counter={@counter}
            phx-value-delta="1"
            id={"#{@id}-inc-1"}
          >
            +1
          </button>
          <button
            type="button"
            class="btn btn-outline btn-sm"
            phx-click="adjust"
            phx-value-counter={@counter}
            phx-value-delta="5"
            id={"#{@id}-inc-5"}
          >
            +5
          </button>
        </div>
        <div class="flex justify-center">
          <button
            type="button"
            class="btn btn-outline btn-sm"
            phx-click="reset"
            phx-value-counter={@counter}
            id={"#{@id}-reset"}
          >
            Reiniciar
          </button>
        </div>
      </div>
    </div>
    """
  end
end
