defmodule Wui3Web.Counter do
  use Wui3Web, :live_view
  alias Wui3Web.Counter.Params
  alias Ecto.Changeset

  @impl true
  def mount(params, _session, socket) do
    changeset = params |> Params.changeset()

    if changeset.valid? do
      {:ok,
       socket
       |> assign(params: changeset |> Changeset.apply_changes())}
    else
      {:ok,
       socket
       |> assign(params: %Params{})
       |> put_flash(:error, "Invalid params using default params values")}
    end
  end

  @impl true
  def handle_params(params, _uri, socket) do
    changeset = Params.changeset(params)

    if changeset.valid? do
      {:noreply, socket |> assign(params: changeset |> Changeset.apply_changes())}
    else
      error_message =
        changeset.errors
        |> Enum.map(fn {field, {message, a}} -> "#{field}: #{message} #{inspect(a)}" end)
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

    count = socket.assigns.params |> Map.get(atom_counter, 0)

    new_count =
      case action do
        "increment" -> count + 1
        "decrement" -> count - 1
      end

    changeset =
      socket.assigns.params
      |> Map.from_struct()
      |> Map.put(atom_counter, new_count)
      |> Params.changeset()

    if changeset.valid? do
      changeset = changeset |> Changeset.apply_changes()

      {:noreply,
       socket
       |> assign(params: changeset)
       |> push_patch(
         to: "/counter?" <> Params.to_query_params(changeset),
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
         "Invalid input for #{Atom.to_string(atom_counter)}. Errors: #{error_message}"
       )}
    end
  end
end
