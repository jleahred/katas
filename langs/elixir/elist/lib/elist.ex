defmodule Elist do
  @moduledoc """
  Documentation for `Elist`.
  """

  use Rustler, otp_app: :elist, crate: "elist"

  def add(_a, _b), do: error()

  def fix_parse(_str), do: error()

  defp error(), do: :erlang.nif_error(:nif_not_loaded)
end

defmodule Example do
  def rust_parse() do
    "8=FIX.4.4|9=148|35=D|34=1080|49=TESTBUY1|52=20180920-18:14:19.508|56=TESTSELL1|11=636730640278898634|15=USD|21=2|38=7000|40=1|54=1|55=MSFT|60=20180920-18:14:19.492|10=092|"
    |> Elist.fix_parse()
  end
end

defmodule FixParse do
  def fix_parse(str) do
    # "8=FIX.4.4|9=148|35=D|34=1080|49=TESTBUY1|..."
    # [
    #   {"8", "FIX.4.4"},
    #   {"9", "148"},
    #   {"35", "D"},
    #   ...
    # ]

    str
    |> String.split("|")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&(&1 |> String.split("=")))
    |> Enum.map(fn [t, v] -> {t, v} end)
  end
end

defmodule Benchmark do
  def run() do
    messages = [
      "8=FIX.4.4|9=148|35=D|34=1080|49=TESTBUY1|52=20180920-18:14:19.508|56=TESTSELL1|11=636730640278898634|15=USD|21=2|38=7000|40=1|54=1|55=MSFT|60=20180920-18:14:19.492|10=092|",
      "8=FIX.4.4|9=289|35=8|34=1090|49=TESTSELL1|52=20180920-18:23:53.671|56=TESTBUY1|6=113.35|11=636730640278898634|14=3500.0000000000|15=USD|17=20636730646335310000|21=2|31=113.35|32=3500|37=20636730646335310000|38=7000|39=1|40=1|54=1|55=MSFT|60=20180920-18:23:53.531|150=F|151=3500|453=1|448=BRK2|447=D|452=1|10=151|",
      "8=FIX.4.2|9=271|35=8|34=974|49=TESTSELL3|52=20190206-16:26:09.059|56=TESTBUY3|6=174.51|11=141636850670842269979|14=100.0000000000|17=3636850671684357979|20=0|21=2|31=174.51|32=100.0000000000|37=1005448|38=100|39=2|40=1|54=1|55=AAPL|60=20190206-16:26:08.435|150=2|151=0.0000000000|10=194|"
    ]

    long_msg =
      "8=FIX.4.4|9=289|35=8|34=1090|49=TESTSELL1|52=20180920-18:23:53.671|56=TESTBUY1|6=113.35|11=636730640278898634|14=3500.0000000000|15=USD|17=20636730646335310000|21=2|31=113.35|32=3500|37=20636730646335310000|38=7000|39=1|40=1|54=1|55=MSFT|60=20180920-18:23:53.531|150=F|151=3500|453=1|448=BRK2|447=D|452=1|10=151|"

    Benchee.run(
      %{
        "elixir_fix" => fn ->
          messages
          |> Enum.map(&(&1 |> Elixir.FixParse.fix_parse()))
        end,
        "rust" => fn ->
          messages
          |> Enum.map(&(&1 |> Elist.fix_parse()))
        end,
        "elixir_fix_ONE" => fn -> Elixir.FixParse.fix_parse(long_msg) end,
        "rust_ONE" => fn -> Elist.fix_parse(long_msg) end
      },
      formatters: [
        Benchee.Formatters.HTML,
        Benchee.Formatters.Console
      ]
    )
  end
end
