defmodule Jle2Web.CounterLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~L"""
    <div>
      <button phx-click="dec">-</button>
      <%= @val %>
      <button phx-click="inc">+</button>
    </div>
    """
  end

  def mount(session, socket) do
    {:ok, assign(socket, :val, session[:val] || 0)}
  end

  def handle_event("inc", _, socket) do
    {:noreply, update(socket, :val, &(&1 + 1))}
  end

  def handle_event("dec", _, socket) do
    {:noreply, update(socket, :val, &(&1 - 1))}
  end
end
