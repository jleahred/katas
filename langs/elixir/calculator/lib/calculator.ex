defmodule Calculator do
  @moduledoc """
  Pending... Documentation for Calculator.

    expr :=  sum (+|-) expr | sum
    sum  :=  factor * sum | factor
    factor := ( expr ) | num
    num  :=  ...

  """

  import NimbleParsec

  @doc """
  Eval

  ## Examples

      iex> Calculator.eval("1+2*3")
      7

  """

  def eval(_s) do
    7
  end

  # @spec to_negative(number()) :: number()
  # defp to_negative(n) do
  #   -1 * n
  # end

  defp first_elem(t) do
    elem(t, 0)
  end

  positive = integer(min: 1)

  # int =
  #   choice([
  #     ignore(ascii_char([?-]))
  #     |> concat(positive)
  #     |> map(:to_negative),
  #     positive
  #   ])

  num =
    positive
    |> optional(
      ignore(ascii_char([?.]))
      |> concat(positive)
    )
    |> reduce({Enum, :join, ["."]})
    |> map({Float, :parse, []})
    |> map(:first_elem)
    |> unwrap_and_tag(:num)

  factor =
    choice([
      ignore(ascii_char([?(]))
      |> parsec(:expr)
      |> ignore(ascii_char([?)])),
      num
    ])

  defcombinatorp(
    :sum,
    choice([
      factor
      |> ignore(ascii_char([?*]))
      |> parsec(:sum)
      |> tag(:mult),
      factor
    ])
  )

  # defp oper2atom(ch_oper) do
  #   case ch_oper do
  #     ?+ -> {:oper, :+}
  #     ?- -> {:oper, :-}
  #   end
  # end

  # operator = ascii_char([?+, ?-]) |> map(:oper2atom)

  defcombinatorp(
    :expr,
    choice([
      parsec(:sum)
      |> ignore(ascii_char([?+]))
      |> parsec(:expr)
      |> tag(:add),
      parsec(:sum)
      |> ignore(ascii_char([?-]))
      |> parsec(:expr)
      |> tag(:subs),
      parsec(:sum)
    ])
  )

  fexpr = parsec(:expr) |> eos

  defparsec(:parse, fexpr)

  defp flat_ast_postfix(num: n) do
    [push_num: n]
  end

  defp flat_ast_postfix([{oper, [exp1, exp2]}]) do
    flat_ast_postfix([exp1]) ++ flat_ast_postfix([exp2]) ++ [exec2: oper]
  end

  @spec test() :: [any()]
  def test do
    inputs = [
      "3.141592653592",
      # "-1+2+3",
      # "-1+2+-3",
      "1+2*3",
      "(1111+2)*3",
      "5+3",
      "1*2",
      "(1*7)+9",
      "1+1+0+3*71",
      "(1982)",
      "0",
      "1+(2+(3*7))",
      # "-1a+2+3",
      "1+2",
      "1-2",
      "1+2+3"
    ]

    for input <- inputs do
      IO.puts(input)
      result = parse(input)
      ast = elem(result, 1)
      IO.inspect(result)
      IO.inspect(flat_ast_postfix(ast))
      IO.puts("")
    end
  end
end

Calculator.test()
