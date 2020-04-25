defmodule JLEWeb.PubSubExLive.GenServer do
  use GenServer
  alias Phoenix.PubSub

  @pubsub :testing_pubsub

  defmodule Status do
    defstruct st: :ready, msg: "init", timestamp: DateTime.utc_now()
  end

  # API
  def update() do
    GenServer.cast(:pub_sub_ex_server, :update)
  end

  def get_status() do
    GenServer.call(:pub_sub_ex_server, :get_status)
  end

  def start_link(default) when is_list(default) do
    GenServer.start_link(__MODULE__, nil, name: :pub_sub_ex_server)
  end

  # Callbacks
  @impl true
  def init(nil) do
    {:ok, %Status{}}
  end

  @impl true
  def handle_call(:get_status, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast(:update, state) do
    if state.st == :ready do
      if finished_recently(state.timestamp) do
        broadcast(
          "My God!!! Someone requiring again???  Last finished was at #{state.timestamp}!!!"
        )

        {:noreply, state}
      else
        async_process()
        msg = "working  #{DateTime.utc_now()}!!!"
        broadcast(msg)
        {:noreply, %Status{st: :working, msg: msg, timestamp: DateTime.utc_now()}}
      end
    else
      broadcast("Someone is boring. Be pacient I started at #{state.timestamp}!!!")
      {:noreply, state}
    end
  end

  defp broadcast(msg) do
    PubSub.broadcast(
      @pubsub,
      "testing_status",
      {:msg, msg}
    )
  end

  @impl true
  def handle_info(:process_finished, _state) do
    msg = "finished  #{DateTime.utc_now()}!!!"
    broadcast(msg)
    {:noreply, %Status{st: :ready, msg: msg, timestamp: DateTime.utc_now()}}
  end

  defp async_process() do
    s = self()

    Task.start_link(fn ->
      Process.sleep(5000)
      send(s, :process_finished)
    end)
  end

  def finished_recently(last) do
    DateTime.diff(DateTime.utc_now(), last, :seconds) < 2
    # false
  end

  # @impl true
  # def handle_cast(w, state) do
  #   {:noreply, state}
  # end
end
