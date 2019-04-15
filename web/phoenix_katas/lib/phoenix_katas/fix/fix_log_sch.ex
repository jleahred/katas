defmodule PhoenixKatas.FixLogSch do
  use Ecto.Schema

  schema "fix_log" do
    # field(:id, :integer)
    field(:dir, :integer, source: :line)
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
end
