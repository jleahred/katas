defmodule Wui4Web.Counter2Live do
  use Wui4Web, :live_view

  def __meta__ do
    %Wui4Web.Helpers.RouterMeta{
      url: "/counter2",
      description: "Interactive two counters with increment and decrement",
      keywords: "counter example",
      grants: [:all]
    }
  end

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count1: 0, count2: 0)}
  end

  def handle_event("inc", %{"idx" => value}, socket) do
    IO.puts(inspect(value))

    field =
      case value do
        "1" -> :count1
        "2" -> :count2
        _ -> :count1
      end

    socket = update(socket, field, &(&1 + 1))
    {:noreply, socket}
  end

  def handle_event("dec", %{"idx" => value}, socket) do
    field =
      case value do
        "1" -> :count1
        "2" -> :count2
        _ -> :count1
      end

    socket = update(socket, field, &(&1 - 1))
    {:noreply, socket}
  end

  # def render(assigns) do
  #   ~H"""
  #   <div style="display: flex; justify-content: center; align-items: center; gap: 1rem; margin-top: 2rem;">
  #     <button class="btn btn-primary" phx-click="dec">-</button>
  #     <span>Counter: {@count}</span>
  #     <.button phx-click="inc" variant="primary">+</.button>
  #   </div>
  #   """
  # end
end
