defmodule Wui7Web.CounterCpLive do
  use Wui7Web, :live_view

  def meta_info do
    %{
      title: "Contador creativo con poderes",
      description:
        "Explora acciones rápidas, multiplicaciones y efectos visuales mientras sincronizas el parámetro count.",
      keywords: ["contador", "creativo", "multiplicar", "aleatorio", "url"]
    }
  end

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    count = parse_count(params["count"])

    socket =
      socket
      |> assign(:count, count)
      |> assign(:color, get_color(count))
      |> assign(:emoji, get_emoji(count))
      |> assign(:message, get_message(count))

    {:noreply, socket}
  end

  @impl true
  def handle_event("add", %{"val" => value}, socket) do
    new_count = socket.assigns.count + String.to_integer(value)
    {:noreply, push_patch(socket, to: ~p"/counter_cp?count=#{new_count}", replace: true)}
  end

  def handle_event("multiply", %{"val" => value}, socket) do
    new_count = socket.assigns.count * String.to_integer(value)
    {:noreply, push_patch(socket, to: ~p"/counter_cp?count=#{new_count}", replace: true)}
  end

  def handle_event("reset", _params, socket) do
    {:noreply, push_patch(socket, to: ~p"/counter_cp", replace: true)}
  end

  def handle_event("random", _params, socket) do
    new_count = Enum.random(-100..100)
    {:noreply, push_patch(socket, to: ~p"/counter_cp?count=#{new_count}", replace: true)}
  end

  defp parse_count(nil), do: 0

  defp parse_count(value) when is_binary(value) do
    case Integer.parse(value) do
      {num, _} -> num
      :error -> 0
    end
  end

  defp get_color(count) when count > 50, do: "success"
  defp get_color(count) when count > 0, do: "info"
  defp get_color(count) when count == 0, do: "neutral"
  defp get_color(count) when count > -50, do: "warning"
  defp get_color(_count), do: "error"

  defp get_emoji(count) when count > 50, do: "🚀"
  defp get_emoji(count) when count > 20, do: "🔥"
  defp get_emoji(count) when count > 0, do: "✨"
  defp get_emoji(0), do: "⚪"
  defp get_emoji(count) when count > -20, do: "❄️"
  defp get_emoji(count) when count > -50, do: "🌊"
  defp get_emoji(_count), do: "💀"

  defp get_message(count) when count > 100, do: "¡ÉPICO!"
  defp get_message(count) when count > 50, do: "Increíble"
  defp get_message(count) when count > 20, do: "Muy bien"
  defp get_message(count) when count > 0, do: "Positivo"
  defp get_message(0), do: "Neutral"
  defp get_message(count) when count > -20, do: "Negativo"
  defp get_message(count) when count > -50, do: "Bajo cero"
  defp get_message(count) when count > -100, do: "Muy bajo"
  defp get_message(_count), do: "CRÍTICO"

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <div class="min-h-screen bg-gradient-to-br from-purple-900 via-blue-900 to-indigo-900 flex items-center justify-center p-4">
        <div class="max-w-4xl w-full">
          <%!-- Main Counter Display --%>
          <div class="text-center mb-8">
            <div class="inline-block">
              <div class={[
                "text-9xl font-black mb-4 transition-all duration-500 transform hover:scale-110",
                "drop-shadow-2xl"
              ]}>
                <span class="animate-pulse">{@emoji}</span>
              </div>
              <div class={[
                "text-8xl font-black mb-4 transition-all duration-300",
                "bg-gradient-to-r from-cyan-400 via-purple-400 to-pink-400 bg-clip-text text-transparent"
              ]}>
                {@count}
              </div>
              <div class={[
                "badge badge-lg gap-2 badge-#{@color} animate-bounce"
              ]}>
                <.icon name="hero-star" class="w-4 h-4" />
                {@message}
              </div>
            </div>
          </div>

          <%!-- Action Grid --%>
          <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
            <%!-- Quick Actions --%>
            <button
              phx-click="add"
              phx-value-val="1"
              id="add-1"
              class="btn btn-lg btn-primary hover:scale-105 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-plus" class="w-6 h-6" /> +1
            </button>

            <button
              phx-click="add"
              phx-value-val="5"
              id="add-5"
              class="btn btn-lg btn-secondary hover:scale-105 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-rocket-launch" class="w-6 h-6" /> +5
            </button>

            <button
              phx-click="add"
              phx-value-val="-1"
              id="sub-1"
              class="btn btn-lg btn-accent hover:scale-105 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-minus" class="w-6 h-6" /> -1
            </button>

            <button
              phx-click="add"
              phx-value-val="-5"
              id="sub-5"
              class="btn btn-lg btn-warning hover:scale-105 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-arrow-down" class="w-6 h-6" /> -5
            </button>
          </div>

          <%!-- Special Operations --%>
          <div class="grid grid-cols-1 md:grid-cols-3 gap-4 mb-8">
            <button
              phx-click="multiply"
              phx-value-val="2"
              id="multiply-2"
              class="btn btn-lg btn-info hover:scale-105 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-x-mark" class="w-6 h-6" /> × 2
            </button>

            <button
              phx-click="random"
              id="random-btn"
              class="btn btn-lg btn-success hover:rotate-12 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-sparkles" class="w-6 h-6" /> Aleatorio
            </button>

            <button
              phx-click="reset"
              id="reset-btn"
              class="btn btn-lg btn-error hover:scale-105 transform transition-all duration-200 shadow-lg"
            >
              <.icon name="hero-arrow-path" class="w-6 h-6" /> Reiniciar
            </button>
          </div>

          <%!-- Stats Cards --%>
          <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div class="stats shadow-xl bg-base-100/80 backdrop-blur-sm">
              <div class="stat">
                <div class="stat-figure text-primary">
                  <.icon name="hero-hashtag" class="w-8 h-8" />
                </div>
                <div class="stat-title">Valor</div>
                <div class="stat-value text-primary">{@count}</div>
                <div class="stat-desc">Desde la URL</div>
              </div>
            </div>

            <div class="stats shadow-xl bg-base-100/80 backdrop-blur-sm">
              <div class="stat">
                <div class="stat-figure text-secondary">
                  <.icon name="hero-calculator" class="w-8 h-8" />
                </div>
                <div class="stat-title">Absoluto</div>
                <div class="stat-value text-secondary">{abs(@count)}</div>
                <div class="stat-desc">Valor sin signo</div>
              </div>
            </div>

            <div class="stats shadow-xl bg-base-100/80 backdrop-blur-sm">
              <div class="stat">
                <div class="stat-figure text-accent">
                  <%= cond do %>
                    <% @count > 0 -> %>
                      <.icon name="hero-arrow-trending-up" class="w-8 h-8" />
                    <% @count < 0 -> %>
                      <.icon name="hero-arrow-trending-down" class="w-8 h-8" />
                    <% true -> %>
                      <.icon name="hero-minus" class="w-8 h-8" />
                  <% end %>
                </div>
                <div class="stat-title">Estado</div>
                <div class="stat-value text-accent">
                  <%= cond do %>
                    <% @count > 0 -> %>
                      Positivo
                    <% @count < 0 -> %>
                      Negativo
                    <% true -> %>
                      Cero
                  <% end %>
                </div>
                <div class="stat-desc">Tendencia actual</div>
              </div>
            </div>
          </div>

          <%!-- URL Info --%>
          <div class="mt-8 alert alert-info shadow-lg">
            <.icon name="hero-information-circle" class="w-6 h-6 shrink-0" />
            <div class="w-full">
              <h3 class="font-bold">Estado sincronizado con URL</h3>
              <div class="text-xs mt-1 font-mono opacity-70 break-all">
                /counter_cp?count={@count}
              </div>
            </div>
          </div>
        </div>
      </div>
    </Layouts.app>
    """
  end
end
