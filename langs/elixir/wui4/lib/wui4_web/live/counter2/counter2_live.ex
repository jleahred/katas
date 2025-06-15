defmodule Wui4Web.Counter2Live do
  use Wui4Web, :live_view
  require Wui4Web.Query

  def __meta__ do
    %Wui4Web.Helpers.RouterMeta{
      url: "/counter2",
      description: "Interactive two counters with increment and decrement",
      keywords: "counter example",
      grants: [:all]
    }
  end

  Wui4Web.Query.defqueryparams Params do
    field :counter1, :integer, default: 1
    field :counter2, :integer, default: 0
    field :text, :string, default: "Hello World"
  end

  def mount(params, _session, socket) do
    {:ok,
     socket
     |> assign(:params, Params.from_query_params(params))}
  end

  def handle_params(params, uri, socket) do
    {:noreply,
     socket
     |> assign(params: Params.from_query_params(params))
     |> assign(current_uri: uri)}
  end

  def handle_event("inc", %{"idx" => value}, socket) do
    params = socket.assigns.params

    {field, new_value} =
      case value do
        "1" -> {:counter1, params.counter1 + 1}
        "2" -> {:counter2, params.counter2 + 1}
        _ -> {:counter1, params.counter1 + 1}
      end

    new_params = Map.put(params, field, new_value)

    current_path = URI.parse(socket.assigns.current_uri).path
    # IO.puts("#{current_path}?#{Params.to_query_string(params)} _________________")

    #   {:noreply,
    #    socket
    #    |> push_patch(to: "#{current_path}?#{query_string}")}
    # end
    {
      :noreply,
      socket
      #  |> assign(params: new_params)
      |> push_patch(to: ~p"/counter2?#{Params.to_query(new_params)}")
    }
  end

  def handle_event("dec", %{"idx" => value}, socket) do
    params = socket.assigns.params

    {field, new_value} =
      case value do
        "1" -> {:counter1, params.counter1 - 1}
        "2" -> {:counter2, params.counter2 - 1}
        _ -> {:counter1, params.counter1 - 1}
      end

    {:noreply, socket |> assign(params: Map.put(params, field, new_value))}
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
