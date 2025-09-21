defmodule Wui6Web.Counter2Live do
  use Wui6Web, :live_view

  @metadata %{
    title: "Dual Counter",
    keywords: "counter live dual sync",
    description: """
    LiveView with two counters kept in sync via the query string.
    Highlights independent updates and a combined total.
    """
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count_a: 0, count_b: 0)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    counts = parse_counts(params)

    {:noreply, assign(socket, counts)}
  end

  @impl true
  def handle_event("increment_a", _params, socket) do
    new_count_a = socket.assigns.count_a + 1

    {:noreply, push_counts(socket, new_count_a, socket.assigns.count_b)}
  end

  def handle_event("decrement_a", _params, socket) do
    new_count_a = socket.assigns.count_a - 1

    {:noreply, push_counts(socket, new_count_a, socket.assigns.count_b)}
  end

  def handle_event("increment_b", _params, socket) do
    new_count_b = socket.assigns.count_b + 1

    {:noreply, push_counts(socket, socket.assigns.count_a, new_count_b)}
  end

  def handle_event("decrement_b", _params, socket) do
    new_count_b = socket.assigns.count_b - 1

    {:noreply, push_counts(socket, socket.assigns.count_a, new_count_b)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} page_title={@page_title} current_user_email={@current_user_email}>
      <div class="space-y-10 pt-4">
        <section class="card bg-base-100 shadow-xl">
          <div class="card-body space-y-8">
            <div class="text-center space-y-2">
              <p class="text-base-content/70 max-w-xl mx-auto">
                Control two independent counters and watch the combined total update instantly.
              </p>
            </div>

            <div class="grid gap-6 md:grid-cols-2">
              <div class="card border border-base-300 bg-base-200/60">
                <div class="card-body items-center text-center space-y-5">
                  <h2 class="card-title text-2xl">Counter A</h2>
                  <span class="stat-value text-5xl">{@count_a}</span>
                  <div class="flex gap-3">
                    <button phx-click="decrement_a" class="btn btn-error btn-lg">-1</button>
                    <button phx-click="increment_a" class="btn btn-success btn-lg">+1</button>
                  </div>
                </div>
              </div>

              <div class="card border border-base-300 bg-base-200/60">
                <div class="card-body items-center text-center space-y-5">
                  <h2 class="card-title text-2xl">Counter B</h2>
                  <span class="stat-value text-5xl">{@count_b}</span>
                  <div class="flex gap-3">
                    <button phx-click="decrement_b" class="btn btn-error btn-lg">-1</button>
                    <button phx-click="increment_b" class="btn btn-success btn-lg">+1</button>
                  </div>
                </div>
              </div>
            </div>

            <div class="alert alert-info justify-center text-lg font-semibold">
              Combined total: {@count_a + @count_b}
            </div>
          </div>
        </section>
      </div>
    </Layouts.app>
    """
  end

  defp parse_counts(params) do
    %{
      count_a: parse_integer_param(params["count_a"], 0),
      count_b: parse_integer_param(params["count_b"], 0)
    }
  end

  defp parse_integer_param(value, default) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} -> int
      _ -> default
    end
  end

  defp parse_integer_param(_value, default), do: default

  defp push_counts(socket, count_a, count_b) do
    socket
    |> assign(count_a: count_a, count_b: count_b)
    |> push_patch(to: ~p"/counter2?#{[count_a: count_a, count_b: count_b]}", replace: true)
  end
end
