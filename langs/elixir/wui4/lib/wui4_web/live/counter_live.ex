defmodule Wui4Web.CounterLive do
  use Wui4Web, :live_view

  def route_path, do: "/countera"
  def route_description, do: "Contador interactivo con incremento y decremento"

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def handle_event("inc", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def handle_event("dec", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
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
