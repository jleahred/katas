defmodule JLEWeb.PubSubExLive do
  use Phoenix.LiveView
  alias Phoenix.PubSub
  alias JLEWeb.PubSubExLive.GenServer, as: Server

  @pubsub :testing_pubsub

  def render(assigns) do
    ~L"""
    <div>
    <button phx-click="click-test" type="button" class="btn btn-primary">Test</button>
    <%= @message %>
    </div>
    """
  end

  def mount(_session, socket) do
    if connected?(socket), do: PubSub.subscribe(@pubsub, "testing_status")
    {:ok, assign(socket, :message, "init")}
  end

  def handle_event("click-test", _, socket) do
    Server.update()
    {:noreply, socket}
  end

  def handle_info({:msg, text}, socket) do
    {:noreply, assign(socket, :message, text)}
  end
end
