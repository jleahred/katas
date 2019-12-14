defmodule Jle2Web.CounterLive do
  use Phoenix.LiveView
  alias Jle2Web.Router.Helpers, as: Routes
  require Logger

  # the status part that will be on url
  defmodule Params do
    defstruct value: 0, value2: 0

    # build %Params from url
    # on non existing fields, it will keep the default value
    def from_url_params(params) do
      %Params{}
      |> struct(
        []
        |> Keyword.put(:value, params |> parse_par_int_or_nil("value"))
        |> Keyword.put(:value2, params |> parse_par_int_or_nil("value2"))
        |> Enum.filter(fn {_k, v} -> v != nil end)
      )
    end

    defp parse_par_int_or_nil(params, skey) do
      case Integer.parse(params |> Map.get(skey, "")) do
        {num, ""} -> num
        _ -> nil
      end
    end
  end

  # status definition with url params fields and others
  defmodule Status do
    defstruct params: %Params{}, description: "none", incremented: 0, decremented: 0
  end

  def render(assigns) do
    ~L"""
    <div>
    Last action: <%= @st.description %><p>
    <button phx-click="dec">-</button>
      <%= @st.params.value %>
      <%= @st.params.value2 %>
      <button phx-click="inc">+</button>
      <p>
      Incremented: <%= @st.incremented %><p>
      Decremented: <%= @st.decremented %><p>
      </div>
    """
  end

  def mount(_session, socket) do
    {:ok, socket |> assign(st: %Status{})}
  end

  def handle_params(params, _uri, socket) do
    Logger.info("#{inspect(socket, pretty: true)}")
    status = socket.assigns.st
    {:noreply, socket |> assign(st: put_in(status.params, Params.from_url_params(params)))}
    # redirect --
    # current_path = "/#{uri |> String.split("/") |> Enum.take(-1)}"
    # Logger.info("__path___  #{current_path}")
    # {:stop,
    #  socket
    #  |> redirect(
    #    to:
    #    "/?orig_path=#{URI.encode_www_form(current_path)}}"
    #  )}
  end

  def handle_event("inc", _id, socket) do
    status = inc_dec(&Kernel.+/2, "incremented", socket.assigns.st)
    status = update_in(status.incremented, &(&1 + 1))

    handle_event_result(socket, status)
  end

  def handle_event("dec", _, socket) do
    status = inc_dec(&Kernel.-/2, "decremented", socket.assigns.st)
    status = update_in(status.decremented, &(&1 + 1))

    handle_event_result(socket, status)
  end

  defp update_url(socket) do
    socket
    |> live_redirect(
      to:
        Routes.live_path(
          socket,
          Jle2Web.CounterLive,
          socket.assigns.st.params |> Map.from_struct()
        )
    )
  end

  defp handle_event_result(socket, status) do
    {:noreply,
     socket
     |> assign(st: status)
     |> update_url()}
  end

  defp inc_dec(oper, action_string, status) do
    status = update_in(status.params.value, &oper.(&1, 1))
    status = update_in(status.params.value2, &oper.(&1, 1))
    status = put_in(status.description, action_string)
    status
  end
end
