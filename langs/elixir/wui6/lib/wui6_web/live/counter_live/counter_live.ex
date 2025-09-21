defmodule Wui6Web.CounterLive do
  use Wui6Web, :live_view

  @metadata %{
    title: "Interactive Counter",
    keywords: "counter live increment decrement test",
    description: """
    Simple LiveView counter that stays in sync with the query string.
    Demonstrates increment and decrement actions.
    """
  }

  def metadata, do: @metadata

  # embed_templates "counter_live/*"

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    {:noreply, assign(socket, :count, count_from_params(params))}
  end

  @impl true
  def handle_event("increment", _params, socket) do
    new_count = socket.assigns.count + 1

    {:noreply, push_count(socket, new_count)}
  end

  def handle_event("decrement", _params, socket) do
    new_count = socket.assigns.count - 1

    {:noreply, push_count(socket, new_count)}
  end

  # @impl true
  # def render(assigns), do: counter(assigns)

  defp count_from_params(params) do
    with count when is_binary(count) <- params["count"],
         {int, ""} <- Integer.parse(count) do
      int
    else
      _ -> 0
    end
  end

  defp push_count(socket, new_count) do
    socket
    |> assign(:count, new_count)
    |> push_patch(to: ~p"/counter1?count=#{new_count}", replace: true)
  end
end
