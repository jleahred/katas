defmodule PhoenixKatasWeb.FactorialLive do
  use Phoenix.LiveView
  require Logger

  def render(assigns) do
    ~L"""
    <div>

    <form phx-change="update">
      <button phx-click="dec">-</button>
        <input name="val" class="" type="number" value=<%= @val %> id="input">
      <button phx-click="inc">+</button>
    </form>

      <h1>The factorial of <%= @val %>  is:</h1>

      <%= for l <- factorial(@val) do %>
      <%= l %>
      <%= end %>

    </div>
    """
  end

  def mount(_session, socket) do
    {:ok, assign(socket, val: 0)}
  end

  def handle_event("inc", _, socket) do
    {:noreply, update(socket, :val, &(&1 + 1))}
  end

  def handle_event("dec", _, socket) do
    Logger.info("received dec")
    {:noreply, update(socket, :val, &(&1 - 1))}
  end

  def handle_event("update", par, socket) do
    Logger.info("received on update #{inspect(par)}")

    {:noreply,
     update(
       socket,
       :val,
       fn val ->
         Logger.info("updating value #{inspect(val)}")
         String.to_integer(par["val"])
       end
     )}
  end

  defp factorial(n) do
    1..n
    |> Enum.reduce(&(&1 * &2))
    |> Integer.to_string()
    |> String.codepoints()
    |> Enum.chunk_every(3)
    |> Enum.map(&Enum.join/1)
  end
end
