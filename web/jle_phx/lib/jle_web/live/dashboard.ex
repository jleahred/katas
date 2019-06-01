defmodule JLEWeb.DashBoardLive do
  use Phoenix.LiveView
  alias JLE.RepoAlarms
  alias JLE.RepoTrading
  import Phoenix.HTML.Link
  require Logger

  # 10 min
  @update_millisecs 10 * 60 * 60 * 1000

  def render(assigns) do
    ~L"""
    <div class="container">
      <h2>DashBoard</h2><p>
      <%= Time.to_string(@date_time) %>
      <p>

      <div class="row">
          <div class="col"><%= render_ceca(assigns) %></div>
          <div class="col"><%= render_abante(assigns) %></div>
      </div>

    </div>
    """
  end

  def mount(_session, socket) do
    if connected?(socket), do: :timer.send_interval(@update_millisecs, self(), :tick)

    {:ok, update_dashboard(socket)}
  end

  defp update_dashboard(socket) do
    assign(socket, date_time: Time.utc_now() |> Time.truncate(:second))
  end

  def handle_info(:tick, socket) do
    {:noreply, update_dashboard(socket)}
  end

  # CECA

  defp get_ceca_versions() do
    query = """
        -- explain
        select max(db_time), broker_code, machine, process_version

        from CLI_ALARMS

        force index(IDX_CLI_ALARMS_DBTSUBJ)

        where DB_TIME > DATE_ADD(current_date, INTERVAL -0 DAY) AND DB_TIME < DATE_ADD(current_date(), INTERVAL +1 DAY)
        AND broker_code = 'CECA'
        group by 2, 3, 4
    """

    Ecto.Adapters.SQL.query!(RepoAlarms, query, []).rows
  end

  defp render_ceca(assigns) do
    get_ceca_color_vers = fn ver ->
      case ver do
        "2.33.2.38" -> ""
        _ -> "bg-danger text-white"
      end
    end

    ~L"""
    <h4>CECA VERSIONS</h4>
    <table class="table table-sm table-hover table-striped small-font">
    <tbody>
        <%= for [dt, _broker, machine, ver] <- get_ceca_versions() do %>
        <tr class="<%=get_ceca_color_vers.(ver)%>">
          <td> <%= dt %></th>
          <td> <%= ver %></th>
          <td> <%= machine %></th>
          </tr>
        <% end %>
    </tbody>
    </table>
    """
  end

  def render_abante(assigns) do
    abante_orders = fn days_back ->
      query = """
          -- explain

          select count(*)

          from TRADING

          force index(IDX_TRD_DBT_)

          where DB_TIME > DATE_ADD(current_date, INTERVAL -#{days_back} DAY)
            AND DB_TIME < DATE_ADD(current_date(), INTERVAL #{days_back + 1} DAY)
            and ORDER_TYPE LIKE 'RQ_NW%'
            AND account_cc = 'ABANTE'
          -- limit 10
      """

      Ecto.Adapters.SQL.query!(RepoTrading, query, []).rows
    end

    ~L"""
    <h4>ABANTE</h4>
    <table class="table table-sm table-hover table-striped small-font">
    <tbody>
    <pre>
    num orders
      today: <%= inspect(abante_orders.(0), pretty: true) %>
      yesterday: <%= inspect(abante_orders.(1), pretty: true) %>
      -3 days: <%= inspect(abante_orders.(-3), pretty: true) %>
    </pre>
    <%= link("fix messages", to: "/live/fix/log?date=#{Date.utc_today |> Date.to_string}&connection=cli_der_blo&msg_type=any&exec_type=any&any=&page=0") %>
    </tbody>
    </table>
    """
  end
end
