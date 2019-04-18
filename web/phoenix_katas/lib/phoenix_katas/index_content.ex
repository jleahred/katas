defmodule PhoenixKatas.IndexContent do
  @index [
    # {"?txt=fix", "testing:search"},
    {"/", "index"},
    # {"/test", "testing"},
    {"/factorial?num=123", "calculate a factorial"},
    {"/fix/tags", "show fix tags"},
    {"/fix/msg_types", "show fix message types"},
    {"/fix/log", "filter and look for fix_log database"},
    {"/fix/log/msg/0", "render a fix message by id (0) for testing"},
    {"/live/counter", "counter smallest live view example (no javascript)"},
    {"/live/factorial", "factorial smallest live view example (no javascript)"}
  ]

  @index_uc @index |> Enum.map(fn {i, d} -> {{i, d}, {String.upcase(i), String.upcase(d)}} end)

  def all() do
    @index_uc
  end
end
