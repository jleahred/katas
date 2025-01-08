defmodule Wui3Web.Counter do
  require Logger
  use Wui3Web, :live_view
  alias Wui3Web.Counter.Params
  alias Ecto.Changeset

  @impl true
  def mount(params, _session, socket) do
    {:ok,
     socket
     |> assign(params: params |> Params.changeset() |> Changeset.apply_changes())}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    Logger.info("handle_params: #{inspect(params)}")

    changeset =
      if map_size(params) == 0 do
        Logger.info("generate_valid_random_params! #{inspect(params)}")
        Params.generate_valid_random_params!()
      else
        Logger.info("NOT EMPTY generate_valid_random_params! #{inspect(params)}")
        params |> Params.changeset()
      end

    # update_params_in_socket_ifso(socket, changeset)

    if changeset.valid? do
      {:noreply, socket |> assign(params: changeset |> Changeset.apply_changes())}
    else
      error_message =
        changeset.errors
        # |> Enum.map(fn {field, {message, a}} -> "#{field}: #{message} #{inspect(a)}" end)
        |> Enum.map(fn {field, {message, _}} -> "#{field}: #{message}" end)
        |> Enum.join(", ")

      {
        :noreply,
        socket
        |> assign(params: %Params{})
        |> put_flash(
          :error,
          "Invalid params using default params values. Errors: #{error_message}"
        )
        #
      }
    end
  end

  @impl true
  def handle_event("change_count", %{"counter" => counter, "action" => action}, socket) do
    atom_counter =
      case counter do
        "1" -> :count1
        "2" -> :count2
      end

    direction =
      case action do
        "increment" -> +1
        "decrement" -> -1
      end

    changeset = socket.assigns.params |> Params.update_counter(atom_counter, direction)

    update_params_in_socket_ifso(socket, changeset)
  end

  def handle_event("change_increment", %{"increment" => increment}, socket) do
    changeset = socket.assigns.params |> Params.update_increment(increment)

    update_params_in_socket_ifso(socket, changeset)
  end

  defp update_params_in_socket_ifso(socket, changeset) do
    if changeset.valid? do
      new_params = changeset |> Changeset.apply_changes()

      {:noreply,
       socket
       |> assign(params: new_params)
       |> push_patch(
         to: ~p"/counter" <> "?#{Params.to_query_params(new_params)}",
         replace: true
       )}
    else
      error_message =
        changeset.errors
        |> Enum.map(fn {field, {message, _}} -> "#{field}: #{message}" end)
        |> Enum.join(", ")

      {:noreply,
       socket
       |> put_flash(
         :error,
         "Errors: #{error_message}"
       )}
    end
  end
end
