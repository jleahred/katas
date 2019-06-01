defmodule JLEWeb.PriceGrantsLive do
  use Phoenix.LiveView
  require Logger

  # @update_millisecs 2000

  def render(assigns) do
    ~L"""
    <div class="container">
    <h1>Prices grants</h1>

    <%= for {_prov, market} <- @rep_info.prov_markets do %>
    <h2><%="#{market}"%></h2>

      <table class="table table-striped">
      <tbody>
          <%= for cup <- filter_market_cup(market, @rep_info.cli_user_prods) do %>
            <tr class="<%=hightlight_row(cup)%>">
            <!-- <td> <%= "#{cup.market}" %></td> -->
            <td> <%= "#{cup.cli}" %></td>
            <td> <%= "#{cup.user}" %></td>
            <td> <%= "#{cup.prods}" %></td>
            <td> <%= "#{cup.last_login}" %></td>
            <td> <%= "#{cup.inactivity_days}" %></td>
            </tr>
          <% end %>
      </tbody>
      </table>
    <%= end %>


    </div>
    """
  end

  def mount(_session, socket) do
    # if connected?(socket), do: :timer.send_interval(@update_millisecs, self(), :tick)

    {:ok, update(socket)}
  end

  # def handle_info(:tick, socket) do
  #   {:noreply, update(socket)}
  # end

  defp update(socket) do
    assign(socket,
      date_time: Time.utc_now() |> Time.truncate(:second),
      rep_info: get_report_info()
    )
  end

  def hightlight_row(cup) do
    cond do
      cup.inactivity_days > 60 -> "bg-danger text-white"
      cup.user == "DANIEL.FERNANDEZ@RF" -> "bg-danger text-white"
      true -> ""
    end
  end

  defp get_map_from_file(file) do
    pid = ExFtp.open("192.168.16.64", "anonymous", "")
    ExFtp.cd(pid, "misc/mk_rep")

    result =
      with {:ok, txt} <- ExFtp.get(pid, file),
           do:
             txt
             |> YamlElixir.read_from_string!()

    ExFtp.close(pid)
    result
  end

  defp filter_market_cup(market, cup) do
    cup
    |> Enum.filter(&(&1.market == market))
    |> Enum.filter(&(&1.user != "JOSELUIS.ESTEBAN@DESARROLLO"))
  end

  defp get_user_list(users_re, cli_code, acs) do
    acs
    |> Enum.map(& &1["user_info"])
    |> Enum.filter(&(&1["client_code"] == cli_code))
    |> Enum.map(& &1["name"])
    |> Enum.filter(&Regex.match?(users_re, &1))
  end

  defp get_product_list(prod_patter_list, provider) do
    provider =
      case provider do
        "EU2" -> "EUREX"
        "M3" -> "MEFF"
        other -> other
      end

    prod_patter_list
    |> Enum.map(
      &case &1 do
        ".*" -> {provider, "all"}
        "(F_|F2_).*$" -> {"XETRA", "all"}
        "F_.*" -> {"XETRA", "all"}
        "F2_.*" -> {"XETRA", "all"}
        "(PA_(?!PX1$).*|AS_(?!AEX$).*|BR_(?!BEL20$).*|LS_(?!PSI20$).*)" -> {"EURONEXT", "all"}
        "^LS_.*" -> {"EURONEXT", "LISBON"}
        "LS_.*" -> {"EURONEXT", "LISBON"}
        other -> {other, "???"}
      end
    )
    |> Enum.uniq()
  end

  defp process_user_info_list(acs, client, provider, uil) do
    uil
    |> Enum.map(fn ui ->
      users = get_user_list(ui["user_pattern"] |> Regex.compile!(), client, acs)
      products = get_product_list(ui["re_product_pattern_list"], provider)

      client =
        case client do
          "CI2" -> "GESTORA"
          other -> other
        end

      %{cli: client, prov: provider, users: users, products: products}
    end)
  end

  defp process_client_info(acs, cc_inf) do
    cc_inf["market_info_list"]
    |> Enum.map(
      &process_user_info_list(
        acs,
        cc_inf["client_code"],
        &1["market"],
        &1["users_info_list"]
      )
    )
  end

  defp get_report_info() do
    acs = get_map_from_file("acs.data")["data"]

    prc_grants = get_map_from_file("grants.yaml")["grants"]

    # prc_grants = %{
    #   "client_code" => "BANKINTER",
    #   "market_info_list" => [
    #     %{
    #       "market" => "M3",
    #       "users_info_list" => [
    #         %{
    #           "last_applied_rule" => "2018-05-22 18:56:17.035",
    #           "re_product_pattern_list" => [".*"],
    #           "user_pattern" => ".*"
    #         }
    #       ]
    #     },
    #     %{
    #       "market" => "EU2",
    #       "users_info_list" => [
    #         %{
    #           "last_applied_rule" => "2018-05-22 18:56:17.035",
    #           "re_product_pattern_list" => [".*"],
    #           "user_pattern" => ".*"
    #         }
    #       ]
    #     },
    #     %{
    #       "market" => "SIBE",
    #       "users_info_list" => [
    #         %{
    #           "last_applied_rule" => "2017-03-22 10:06:22.930",
    #           "re_product_pattern_list" => [".*"],
    #           "user_pattern" => "DAVID@BANKINTER"
    #         }
    #       ]
    #     },
    #     %{
    #       "market" => "VT",
    #       "users_info_list" => [
    #         %{
    #           "last_applied_rule" => "2017-03-22 10:06:22.930",
    #           "re_product_pattern_list" => [
    #             "(F_|F2_).*$",
    #             "(PA_(?!PX1$).*|AS_(?!AEX$).*|BR_(?!BEL20$).*|LS_(?!PSI20$).*)"
    #           ],
    #           "user_pattern" => "DAVID@BANKINTER"
    #         }
    #       ]
    #     }
    #   ]
    # }

    cli_prod_grants =
      prc_grants
      |> Enum.map(&process_client_info(acs, &1))
      |> List.flatten()
      |> Enum.filter(&(length(&1[:users]) != 0))

    prov_markets =
      cli_prod_grants
      |> Enum.map(fn clprodg ->
        prov = clprodg.prov
        clprodg.products |> Enum.map(&{prov, elem(&1, 0)})
      end)
      |> List.flatten()
      |> Enum.uniq()
      |> Enum.filter(fn {prov, _} -> prov != "EU" end)

    today = Date.utc_today()

    cli_user_prods =
      cli_prod_grants
      |> Enum.map(fn cli_prod_users ->
        for cli <- [cli_prod_users.cli],
            user <- cli_prod_users.users,
            {market, prods} <- cli_prod_users.products do
          last_login = get_last_login(user, acs)

          %{
            cli: cli,
            user: user,
            market: market,
            prods: prods,
            last_login: last_login,
            inactivity_days: Date.diff(today, last_login)
          }
        end
      end)
      |> List.flatten()

    %{
      prov_markets: prov_markets,
      cli_user_prods: cli_user_prods,
      temp: acs
    }
  end

  defp get_last_login(user, acs) do
    acs
    |> Enum.map(&%{user: &1["user_info"]["name"], last_login: &1["last_access"]})
    |> Enum.filter(&(&1.user == user))
    |> Enum.map(& &1.last_login)
    |> List.first()
    |> String.split(" ")
    |> (fn [d, t] -> "#{d}" end).()
    |> Date.from_iso8601!()
  end
end
