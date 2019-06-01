defmodule JLEWeb.MainPageLive do
  use Phoenix.LiveView
  require Logger

  def render(assigns) do
    ~L"""
    <div class="container">
    <div>

      <%#= inspect assigns %>
      <div>
          <form phx-change="search_update"
            class="form-inline
            my-2
            my-lg-0"
            onkeypress="return event.keyCode != 13;"
          >
            <input id="txt" class="form-control mr-sm-2" type="search" value="<%=@txt%>" placeholder="Search" aria-label="Search" name="txt" autofocus autocomplete="off">
            <%# <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Go!</button> %>
          </form>
      </div>

      <p>
      <div phx-keydown="keydown"
      phx-target="window"
      >
        <table class="table table-hover">
          <tbody>
              <%= for {{route, description}, idx} <- @rows |> Enum.with_index do %>
                <tr class='clickable-row <%= if @current_row == idx, do: "table-primary", else: ""%>'
                  data-href='<%= route %>'
                >
                  <%#th scope="row"> < %= route % ></th%>
                  <td> <%= description %></td>
                </tr>
              <% end %>
          </tbody>
        </table>
      </div>

    </div>
    </container>

    <script>
    document.getElementById("txt").select();
    </script>
    """
  end

  def mount(_session, socket) do
    {:ok,
     assign(socket,
       txt: "",
       current_row: 0,
       rows: search("")
     )}
  end

  def handle_event("search_update", par, socket) do
    {
      :noreply,
      socket
      |> update(
        :txt,
        fn _txt -> par["txt"] end
      )
      |> update(
        :rows,
        fn _ -> search(par["txt"]) end
      )
      |> update(
        :current_row,
        fn _ -> 0 end
      )
    }
  end

  def handle_event("keydown", key, socket) do
    current_row = socket.assigns.current_row

    if key == "Enter" do
      Logger.info(
        "received key ENTER #{inspect(socket.assigns.rows |> Enum.at(current_row) |> elem(0))}"
      )

      {
        :stop,
        socket
        # |> update(:goto_link, fn _ -> socket.assigns.rows |> Enum.at(current_row) |> elem(0) end)
        |> redirect(to: socket.assigns.rows |> Enum.at(current_row) |> elem(0))
      }
    else
      move_to =
        case {key, current_row == 0, current_row == length(socket.assigns.rows) - 1} do
          {"ArrowUp", false, _} -> -1
          {"ArrowDown", _, false} -> +1
          _ -> 0
        end

      {:noreply, update(socket, :current_row, fn r -> r + move_to end)}
    end
  end

  defp search(txt) do
    words = String.split(txt) |> Enum.map(&(&1 |> String.upcase()))

    # JLE.IndexContent.all returns
    # {{i,c}, {I,C}} second element is uppercase
    all = JLE.IndexContent.all()

    words
    |> Enum.reduce(all, fn w, acc -> filter_word(acc, w) end)
    |> Enum.map(fn %{orig: orig} -> orig end)
  end

  defp filter_word(list, w) do
    list
    |> Enum.filter(fn %{upcase: {idx, desc}} ->
      String.contains?(idx, w) or String.contains?(desc, w)
    end)
  end
end
