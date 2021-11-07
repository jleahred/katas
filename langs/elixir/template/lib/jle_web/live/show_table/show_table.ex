defmodule JleWeb.ShowTable do
  # use JleWeb, :live_view
  use Phoenix.Component

  def list_fields(data, remove_fields) do
    data
    |> Enum.at(0)
    |> Map.from_struct()
    |> Enum.map(fn {k, _v} -> k end)
    |> Enum.filter(&(!(&1 in remove_fields)))
  end

  def list_values(data, remove_fields) do
    data
    |> Map.from_struct()
    |> Enum.filter(fn {k, _} -> !(k in remove_fields) end)
    |> Enum.map(fn {_k, v} -> v end)
  end

  def show_table(assigns) do
    ~H"""
    <div class="table-container">
    <table class="table">
    <%= for field_name <- @data |> list_fields(@remove_fields) do %>
    <th>
        <%= field_name %>
    </th>
    <% end %>

    <%= for row <- @data do%>
    <tr>
        <%= for field <- row |> list_values(@remove_fields) do %>
        <td>
            <%= inspect(field, pretty: true) %>
        </td>
        <% end %>
    </tr>
    <% end %>


    </table>
    </div>
    """
  end
end
