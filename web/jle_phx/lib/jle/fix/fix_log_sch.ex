defmodule JLE.FixLogSch do
  use Ecto.Schema

  schema "fix_log" do
    # field(:id, :integer)
    field(:time, :string)
    field(:connection, :string, source: :client)
    field(:msgtype, :string)
    field(:exectype, :string)
    field(:clordid, :string)
    field(:origclordid, :string)
    field(:symbol, :string)
    field(:securityid, :string)
    field(:side, :integer)
    field(:account, :string)
    field(:price, :string)
    field(:quantity, :integer, source: :orderqty)
    field(:timeinforce, :string)
    field(:fix, :string)
  end

  def normalize_record(fix_msg) do
    fix_msg
    |> Map.update!(:fix, &String.replace(&1, <<01>>, "|"))
    |> Map.update!(:msgtype, &Fix.Static.MsgTypes.get_name(&1))
    |> Map.update!(:exectype, &Fix.Static.ExecTypes.get_name(&1))
    |> Map.update!(:time, &String.slice(&1, 11..50))
    |> Map.update!(:timeinforce, &Fix.Static.TimeInForce.get_name(&1))
    |> Map.update!(
      :side,
      &case &1 do
        1 -> "B"
        2 -> "S"
        _ -> ""
      end
    )
  end
end
