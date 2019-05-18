defmodule PhoenixKatas.IndexContent do
  @index [
    # {"?txt=fix", "testing:search"},
    # {"/", "index"},
    # {"/test", "testing"},
    {"/factorial?num=123", "calculate a factorial"},
    {"/fix/static_tables/currencies", "show static table for currencies"},
    {"/fix/static_tables/msg_types", "show static table for message types"},
    {"/fix/static_tables/exec_types", "show static table for execs types"},
    {"/live/fix/log", "(live)  filter and look for fix_log database"},
    {"/fix/log", "OLD filter and look for fix_log database"},
    {"/fix/log/msg/0", "render a fix message by id (0) for testing"},
    {"/fix/log/clordid/?clordid=J4HDA00CY0002&connection=dest_vt_mifid&date=2019-04-17",
     "messages related to clordid"},
    {"/live/counter", "(live) counter smallest live view example (no javascript)"},
    {"/live/factorial", "(live) factorial smallest live view example (no javascript)"},
    {"/cws/versions", "cws versions running"},
    {"/example/graph", "Graph example"},
    {"/batchs", "batchs processes output"}
  ]

  @index_uc @index |> Enum.map(fn {i, d} -> {{i, d}, {String.upcase(i), String.upcase(d)}} end)

  def all() do
    @index_uc
  end
end
