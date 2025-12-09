defmodule Wui7Web.CounterCp2Live do
  use Wui7Web, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket |> assign(:current_scope, nil)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    counter_a = parse_count(params["a"])
    counter_b = parse_count(params["b"])
    
    socket =
      socket
      |> assign(:counter_a, counter_a)
      |> assign(:counter_b, counter_b)
    
    {:noreply, socket}
  end

  @impl true
  def handle_event("increment", %{"counter" => counter}, socket) do
    new_value = Map.get(socket.assigns, String.to_atom("counter_#{counter}")) + 1
    {:noreply, update_counter(socket, counter, new_value)}
  end

  def handle_event("decrement", %{"counter" => counter}, socket) do
    new_value = Map.get(socket.assigns, String.to_atom("counter_#{counter}")) - 1
    {:noreply, update_counter(socket, counter, new_value)}
  end

  def handle_event("reset", %{"counter" => counter}, socket) do
    {:noreply, update_counter(socket, counter, 0)}
  end

  defp update_counter(socket, counter, value) do
    counter_a = if counter == "a", do: value, else: socket.assigns.counter_a
    counter_b = if counter == "b", do: value, else: socket.assigns.counter_b
    
    push_patch(socket, to: ~p"/counter_cp2?a=#{counter_a}&b=#{counter_b}")
  end

  defp parse_count(nil), do: 0
  defp parse_count(value) when is_binary(value) do
    case Integer.parse(value) do
      {num, _} -> num
      :error -> 0
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <div class="space-y-8">
        <div class="text-center">
          <h1 class="text-4xl font-bold mb-2">Doble Contador</h1>
          <p class="text-base-content/70">Dos contadores independientes sincronizados con la URL</p>
        </div>

        <div class="grid md:grid-cols-2 gap-6">
          <%!-- Counter A --%>
          <div class="card bg-primary/10 border border-primary/20 shadow-lg">
            <div class="card-body items-center text-center">
              <h2 class="card-title text-primary">Contador A</h2>
              
              <div class="py-8">
                <div class="text-7xl font-black text-primary">
                  {@counter_a}
                </div>
              </div>

              <div class="card-actions justify-center gap-2">
                <button
                  phx-click="decrement"
                  phx-value-counter="a"
                  id="decrement-a"
                  class="btn btn-circle btn-primary"
                >
                  <.icon name="hero-minus" class="w-6 h-6" />
                </button>
                <button
                  phx-click="reset"
                  phx-value-counter="a"
                  id="reset-a"
                  class="btn btn-circle btn-ghost"
                >
                  <.icon name="hero-arrow-path" class="w-6 h-6" />
                </button>
                <button
                  phx-click="increment"
                  phx-value-counter="a"
                  id="increment-a"
                  class="btn btn-circle btn-primary"
                >
                  <.icon name="hero-plus" class="w-6 h-6" />
                </button>
              </div>
            </div>
          </div>

          <%!-- Counter B --%>
          <div class="card bg-secondary/10 border border-secondary/20 shadow-lg">
            <div class="card-body items-center text-center">
              <h2 class="card-title text-secondary">Contador B</h2>
              
              <div class="py-8">
                <div class="text-7xl font-black text-secondary">
                  {@counter_b}
                </div>
              </div>

              <div class="card-actions justify-center gap-2">
                <button
                  phx-click="decrement"
                  phx-value-counter="b"
                  id="decrement-b"
                  class="btn btn-circle btn-secondary"
                >
                  <.icon name="hero-minus" class="w-6 h-6" />
                </button>
                <button
                  phx-click="reset"
                  phx-value-counter="b"
                  id="reset-b"
                  class="btn btn-circle btn-ghost"
                >
                  <.icon name="hero-arrow-path" class="w-6 h-6" />
                </button>
                <button
                  phx-click="increment"
                  phx-value-counter="b"
                  id="increment-b"
                  class="btn btn-circle btn-secondary"
                >
                  <.icon name="hero-plus" class="w-6 h-6" />
                </button>
              </div>
            </div>
          </div>
        </div>

        <%!-- Combined Stats --%>
        <div class="card bg-base-100 border border-base-300 shadow-lg">
          <div class="card-body">
            <h3 class="card-title">Estadísticas Combinadas</h3>
            <div class="stats stats-vertical sm:stats-horizontal shadow">
              <div class="stat">
                <div class="stat-title">Suma Total</div>
                <div class="stat-value text-accent">{@counter_a + @counter_b}</div>
              </div>
              
              <div class="stat">
                <div class="stat-title">Diferencia</div>
                <div class="stat-value text-info">{abs(@counter_a - @counter_b)}</div>
              </div>
              
              <div class="stat">
                <div class="stat-title">Producto</div>
                <div class="stat-value text-success">{@counter_a * @counter_b}</div>
              </div>
            </div>
          </div>
        </div>

        <%!-- URL Info --%>
        <div class="alert alert-info">
          <.icon name="hero-link" class="w-6 h-6" />
          <div>
            <h3 class="font-bold">URL Sincronizada</h3>
            <div class="text-xs font-mono opacity-70">/counter_cp2?a={@counter_a}&b={@counter_b}</div>
          </div>
        </div>
      </div>
    </Layouts.app>
    """
  end
end
