defmodule PhoenixKatasWeb.MainPageLive do
  use Phoenix.LiveView
  require Logger

  def render(assigns) do
    ~L"""
    <div>
    <%#= inspect assigns %>
    <div class="d-flex justify-content-end">
        <form phx-change="search_update" class="form-inline my-2 my-lg-0" action="/">
          <input id="txt" class="form-control mr-sm-2" type="search" value="<%=@txt%>" placeholder="Search" aria-label="Search" name="txt" autofocus>
          <%# <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Go!</button> %>
        </form>
    </div>


    <table class="table table-hover">
      <tbody>
          <%= for {route, description} <- search(@txt) do %>
            <tr  class='clickable-row' data-href='<%= route %>'>
              <th scope="row"> <%= route %></th>
              <td> <%= description %></td>
            </tr>
          <% end %>
      </tbody>
    </table>

    </div>

    <script>
    document.getElementById("txt").select();
    </script>
    """
  end

  def mount(_session, socket) do
    Logger.info("sesion #{inspect(socket)}")
    {:ok, assign(socket, txt: "")}
  end

  def handle_event("search_update", par, socket) do
    Logger.info("received on search_update #{inspect(par)}")

    {:noreply,
     update(
       socket,
       :txt,
       fn _txt ->
         #  Logger.info("updating value #{inspect(txt)}")
         par["txt"]
       end
     )}
  end

  defp search(txt) do
    String.split(txt)
    |> Enum.reduce(PhoenixKatas.IndexContent.all(), fn w, acc ->
      filter_word(acc, String.upcase(w))
    end)
    |> Enum.map(fn {orig, _} -> orig end)
  end

  defp filter_word(list, w) do
    list
    |> Enum.filter(fn {_orig, {idx, desc}} ->
      String.contains?(idx, w) or String.contains?(desc, w)
    end)
  end
end
