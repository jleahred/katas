defmodule JLE.IndexContent do
  @index [
    # {"?txt=fix", "testing:search"},
    # {"/", "index"},
    # {"/test", "testing"},
    {"/dashboard", "dashboard"},
    {"/factorial?num=123", "calculate a factorial"},
    {"/fix/static_tables/currencies", "(fix) show static table for currencies"},
    {"/fix/static_tables/msg_types", "(fix) show static table for message types"},
    {"/fix/static_tables/exec_types", "(fix) show static table for execs types"},
    {"/live/fix/log", "(fix)(live)  filter and look for fix_log database"},
    {"/fix/log", "OLD filter and look for fix_log database"},
    {"/fix/log/msg/0", "(fix) render a fix message by id (0) for testing"},
    {"/live/counter", "(live) counter smallest live view example (no javascript)"},
    {"/live/factorial", "(live) factorial smallest live view example (no javascript)"},
    {"/cws/versions", "(cws) versions running"},
    {"/fix/log/clordid/?clordid=19042998543529854352FA00JH0002&connection=dest_sibe_eq_bmex&date=2019-04-29",
     "(fix) messages related to clordid"},
    {"/example/graph", "Graph example"},
    {"/cws/stats/delay", "cimd work station delays (local:pp)"},
    {"/logs", "logs processes output"},
    {"/price_grants", "Prices grants"},
    {"/fix/beautifier", "FIX protocol beautifier"},
    {"/pubsub/ex", "Pubsub simple example"}
  ]

  @index_uc @index
            |> Enum.map(fn {i, d} ->
              %{orig: {i, d}, upcase: {String.upcase(i), String.upcase(d)}}
            end)

  def all() do
    @index_uc
  end
end
