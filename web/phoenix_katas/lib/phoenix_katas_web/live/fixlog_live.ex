defmodule PhoenixKatasWeb.FixLogLive do
  use Phoenix.LiveView
  require Logger

  def mount(session, socket) do
    {:ok,
     assign(socket,
       form: session.params,
       records: session.records,
       fix_msg:
         (session.records |> Enum.at(0, %{fix: ""})).fix
         |> parse_fix_msg,
       current_row: 0
     )}
  end

  def handle_event("click-idx-" <> srow, _, socket) do
    row = String.to_integer(srow)

    {
      :noreply,
      socket
      |> update(:fix_msg, fn _ ->
        Enum.at(socket.assigns.records, row).fix
        |> parse_fix_msg
      end)
      |> update(:current_row, fn _ -> row end)
    }
  end

  def handle_event("keydown", key, socket) do
    current_row = socket.assigns.current_row

    next_row =
      case {key, current_row == 0, current_row == length(socket.assigns.records) - 1} do
        {"ArrowUp", false, _} -> current_row - 1
        {"ArrowDown", _, false} -> current_row + 1
        _ -> current_row
      end

    {
      :noreply,
      socket
      |> update(:current_row, fn _ -> next_row end)
      |> update(:fix_msg, fn _ ->
        Enum.at(socket.assigns.records, next_row).fix |> parse_fix_msg
      end)
    }
  end

  def handle_event(unknown_event, context, socket) do
    Logger.info("Recieve unknown event #{unknown_event} context: #{context}")

    {:noreply, socket}
  end

  def render(assigns) do
    ~L"""
    <div style="position:fixed;background:white;width:100%;">
      <div>
      <%= render_form(assigns) %>
      </div>

      <div>
        <div class="row">
          <div class="col-lg-9">
            <div>
            <!-- <div phx-keydown="keydown"
              phx-target="window"
            > -->
                <%= render_table(assigns) %>
            </div>
          </div>
          <div class="col-lg-3">
              <%= render_msg(assigns) %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def render_msg(assigns) do
    ~L"""
    <div class="table_msg_scroll">
          <table class="table table-sm table-hover table-striped">
          <tbody>
              <%= for {tag, tname, val} <- @fix_msg do %>
                  <tr class="small-font">
                  <th scope="row"> <%= tag %></th>
                  <td> <%= tname %></td>
                  <td> <%= val_riched(tag, val) %></td>
                  </tr>
              <% end %>
          </tbody>
          </table>

    </div>
    """
  end

  def render_form(assigns) do
    ~L"""
    <form name="filter" id='filter' class="form-inline">
        <input class="form-control input-group-lg reg_name"
                type="date"
                name="date"
                title="Day"
                placeholder="Date"
                value="<%= @form["date"] %>"
                />

        <input class="form-control"
            type="search"
            list="connections"
            name="connection"
            value= "<%=@form["connection"]%>"
            >
        <datalist id="connections">
        <option value="any"     <%=if @form["connection"] == "any" do "selected" else "" end %> > Any </option>
        <%= for c <- Fix.Static.Clients.all() do %>
            <option value="<%= c %>"     <%=if @form["connection"] == c do "selected" else "" end %> > <%= c %> </option>
        <% end %>
        </datalist>
        </input>

        <!--  <select class="selectpicker" data-live-search="true"
        <select class="form-control" data-live-search="true"
            name="connection"
            id="idconn">

            <option value="any"     <%=if @form["connection"] == "any" do "selected" else "" end %> > Any </option>
            <%= for c <- Fix.Static.Clients.all() do %>
                <option value="<%= c %>"     <%=if @form["connection"] == c do "selected" else "" end %> > <%= c %> </option>
            <% end %>

        </select>  -->
        <!--   <select class="selectpicker"  data-live-search="true -->
        <select class="form-control"
            name="msg_type"
            id="idmsgtype">

            <option value="any"     <%=if @form["msg_type"] == "any" do "selected" else "" end %> > Any </option>
            <%= for c <- Fix.Static.MsgTypes.list_all_codes do %>
                <option value="<%= c %>"     <%=if @form["msg_type"] == c do "selected" else "" end %> > <%= Fix.Static.MsgTypes.get_name(c) %> </option>
            <% end %>
        </select>

        <select class="form-control"
            name="exec_type"
            id="idexectype">

            <option value="any"     <%=if @form["exec_type"] == "any" do "selected" else "" end %> > Any </option>
            <%= for c <- Fix.Static.ExecTypes.list_all_codes do %>
                <option value="<%= c %>"     <%=if @form["exec_type"] == c do "selected" else "" end %> > <%= Fix.Static.ExecTypes.get_name(c) %> </option>
            <% end %>
        </select>

        <!-- <input id="content" class="form-control input-group-lg reg_name" type="search" name="any"  -->
        <input id="content" class="form-control input-group-lg reg_name" type="search" name="any" autocomplete="off"
            title="Find in any field"
                placeholder="Find in any field"
                value="<%= @form["any"] %>"
                />

        <button onclick="reduce_page()" id="prev" class="btn btn-outline-prim my-2 my-sm-0 pl-4" type="button"> < </button>
        <input id="page"
                class="form-control input-group-lg page-input col-1 center"
                type="number"
                name="page"
                title="Page"
                placeholder="0"
                min="0"
                value="<%= @form["page"] %>"
                />
        <button onclick="increase_page()" id="next" class="btn btn-outline-prim my-2 my-sm-0 pr-4" type="button"> > </button>

        <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Go!</button>

    </form>
    <script>
      function reduce_page() {
          cv = document.getElementById('page').value = document.getElementById('page').value * 1
          if(cv > 0) {
            document.getElementById('page').value = document.getElementById('page').value*1 - 1;
          }
          document.getElementById("filter").submit();
      }
      function increase_page() {
          document.getElementById('page').value = document.getElementById('page').value*1 + 1;
          document.getElementById("filter").submit();
      }
      document.getElementById('idmsgtype').style.display = "block";
      document.getElementById('idconn').style.display = "block";
    </script>
    """
  end

  def render_table(assigns) do
    ~L"""
    <div class="table-wrapper-scroll-y my-custom-scrollbar">
    <table class="table table-sm table-hover table-striped localScroll table_fixed">
      <tbody>
            <tr>
              <th> time </th>
              <th class="text-truncate"> connection</th>
              <th class="text-truncate"> msgtype</th>
              <th class="text-truncate"> exectype</th>
              <th class="text-truncate"> clordid</th>
              <th class="text-truncate"> origclordid</th>
              <th class="text-truncate"> securityid</th>
              <th class="text-truncate"> symbol</th>
              <th class="text-truncate"> side</th>
              <th class="text-truncate"> account</th>
              <th class="text-truncate"> price</th>
              <th> quantiy</th>
              <th> timeinforce</th>
            </tr>
          <%= for {r, idx} <- @records |> Enum.with_index do %>
            <tr  class='small-font <%= if @current_row == idx, do: "table-primary", else: ""%>'
                 phx-click=<%= "click-idx-#{idx}"
                 %> >
              <td><a href="/fix/log/msg/<%= r.id %>">  <%= r.time %> </a></td>
              <td class="text-truncate"> <%= r.connection %></td>
              <td class="text-truncate"> <%= r.msgtype %></td>
              <td class="text-truncate"> <%= r.exectype %></td>
              <td class="text-truncate"> <%= r.clordid %></td>
              <td class="text-truncate"> <%= r.origclordid %></td>
              <td class="text-truncate"> <%= r.securityid %></td>
              <td class="text-truncate"> <%= r.symbol %></td>
              <td class="text-truncate"> <%= r.side %></td>
              <td class="text-truncate"> <%= r.account %></td>
              <td> <%= r.price %></td>
              <td> <%= r.quantity %></td>
              <td> <%= r.timeinforce %></td>
            </tr>
          <% end %>
        <tr>
      </tbody>
    </table>
    </div>
    """
  end

  def parse_fix_msg(fix_msg) do
    fix_msg
    |> String.split("|")
    |> Stream.filter(&(&1 != "\n"))
    |> Stream.map(&String.split(&1, "="))
    |> Stream.filter(&(&1 != [""]))
    |> Stream.map(fn [tag, val] -> {tag, Fix.Static.Tags.get_name(tag), val} end)
    |> Enum.into([])
  end

  def val_riched(tag, val) do
    case tag do
      "35" -> "#{val} #(#{Fix.Static.MsgTypes.get_name(val)})"
      "150" -> "#{val} #(#{Fix.Static.ExecTypes.get_name(val)})"
      "59" -> "#{val} #(#{Fix.Static.TimeInForce.get_name(val)})"
      "54" -> "#{val} #(#{side_text(val)})"
      "15" -> "#{val} #(#{Fix.Static.Currencies.get_name(val)})"
      _ -> "#{val}"
    end
  end

  def side_text(s) do
    case s do
      "1" -> "Buy"
      "2" -> "Sell"
      _ -> ""
    end
  end
end
