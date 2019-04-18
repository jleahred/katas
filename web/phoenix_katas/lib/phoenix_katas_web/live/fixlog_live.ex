defmodule PhoenixKatasWeb.FixLogLive do
  use Phoenix.LiveView
  require Logger

  def mount(session, socket) do
    {:ok,
     assign(socket,
       form: session.params,
       records: session.records,
       fix_msg: Enum.at(session.records, 0).fix |> parse_fix_msg,
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
      |> update(:fix_msg, fn _ -> fake_msg(next_row) |> parse_fix_msg end)
    }
  end

  def render(assigns) do
    ~L"""
    <%#div style="position:fixed;background:white;width:100%;"%>
      <div>
      <%= render_form(assigns) %>
      </div>

      <div>
        <div class="row">
          <div class="col-lg-9">
            <%#div phx-keydown="keydown"
              phx-target="window"
            %>
                <%= render_table(assigns) %>
            <%#/div%>
          </div>
          <div class="col-lg-1">
              <%= render_msg(assigns) %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def render_msg(assigns) do
    ~L"""
    <div>
          <table class="table table-sm table-hover table-striped">
          <tbody>
              <%= for {tag, tname, val} <- @fix_msg do %>
                  <tr>
                  <th scope="row"> <%= tag %></th>
                  <td> <%= tname %></td>
                  <td> <%= val %></td>
                  </tr>
              <% end %>
          </tbody>
          </table>

    </div>
    """
  end

  def render_form(assigns) do
    ~L"""
    <form name="registration_form" id='registration_form' class="form-inline">
    <!--  par example %{"any" => "abc", "connection" => "bloomb", "date" => "2019-04-17", "dir" => "out", "msg_type" => "bbva"} -->

    <input class="form-control input-group-lg reg_name"
            type="date"
            name="date"
            title="Insert the day"
            placeholder="Date"
            value="<%= @form["date"] %>"
            />

    <select class="form-control"
                name="dir">
        <option value="both" <%=if @form["dir"] == "both" do "selected" else "" end %> >Both</option>
        <option value="in"   <%=if @form["dir"] == "in"   do "selected" else "" end %>> In</option>
        <option value="out"  <%=if @form["dir"] == "out"  do "selected" else "" end %> >Out</option>
    </select>

    <%#input list="lconnections" class="form-control">
    <datalist id="lconnections">
      <option value="Internet Explorer">
      <option value="Firefox">
      <option value="Chrome">
      <option value="Opera">
      <option value="Safari">
    </datalist>
    </input%>

    <select class="selectpicker" data-live-search="true"
        name="connection">

        <option value="any"     <%=if @form["connection"] == "any" do "selected" else "" end %> > Any </option>
        <%= for c <- Fix.Static.Clients.all() do %>
            <option value="<%= c %>"     <%=if @form["connection"] == c do "selected" else "" end %> > <%= c %> </option>
        <% end %>

    </select>

    <select class="selectpicker" data-live-search="true"
        name="msg_type">

        <option value="any"     <%=if @form["msg_type"] == "any" do "selected" else "" end %> > Any </option>
        <%= for c <- Fix.Static.MsgTypes.list_all_codes do %>
            <option value="<%= c %>"     <%=if @form["msg_type"] == c do "selected" else "" end %> > <%= Fix.Static.MsgTypes.get_name(c) %> </option>
        <% end %>
    </select>


    <input id="content" class="form-control input-group-lg reg_name" type="search" name="any"
            title="Find in any field"
            placeholder="Find in any field"
            value="<%= @form["any"] %>"
            />

    <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Go!</button>

    </form>
    """
  end

  def render_table(assigns) do
    ~L"""
      <table class="table table-sm table-hover table-striped localScroll">
      <tbody>
            <tr>
              <th> time </th>
              <th> connection</th>
              <th> msgtype</th>
              <th> exectype</th>
              <th> clordid</th>
              <th> origclordid</th>
              <th> securityid</th>
              <th> symbol</th>
              <th> side</th>
              <th> account</th>
              <th> price</th>
              <th> quantiy</th>
              <th> timeinforce</th>
            </tr>
          <%= for {r, idx} <- @records |> Enum.with_index do %>
            <tr  class='<%= if @current_row == idx, do: "table-primary", else: ""%>'
                 phx-click=<%= "click-idx-#{idx}"
                 %> >
              <td><a href="/fix/log/msg/0">  <%= r.time %> </a></td>
              <td> <%= r.connection %></td>
              <td> <%= r.msgtype %></td>
              <td> <%= r.exectype %></td>
              <td> <%= r.clordid %></td>
              <td> <%= r.origclordid %></td>
              <td> <%= r.securityid %></td>
              <td> <%= r.symbol %></td>
              <td> <%= r.side %></td>
              <td> <%= r.account %></td>
              <td> <%= r.price %></td>
              <td> <%= r.quantity %></td>
              <td> <%= r.timeinforce %></td>
            </tr>
          <% end %>
        <tr>
      </tbody>
    </table>
    """
  end

  def fake_msg(row) do
    case row do
      0 ->
        "8=FIX.4.4|9=126|35=A|49=theBroker.12345|56=CSERVER|34=1|52=20170117- 08:03:04|57=TRADE|50=any_string|98=0|108=30|141=Y|553=12345|554=passw0rd!|10=131|"

      1 ->
        "8=FIX.4.5|9=126|35=A|49=theBroker.12345|56=CSERVER|34=1|52=20170117- 08:03:04|57=TRADE|50=any_string|98=0|108=30|141=Y|553=12345|554=passw0rd!|10=131|"

      2 ->
        "8=FIX.4.6|9=126|35=A|49=theBroker.12345|56=CSERVER|34=1|52=20170117- 08:03:04|57=TRADE|50=any_string|98=0|108=30|141=Y|553=12345|554=passw0rd!|10=131|"

      _ ->
        "8=FIX.4.7|9=126|35=A|49=theBroker.12345|56=CSERVER|34=1|52=20170117- 08:03:04|57=TRADE|50=any_string|98=0|108=30|141=Y|553=12345|554=passw0rd!|10=131|"
    end
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
end
