defmodule Calculator.Helpers do
  import NimbleParsec

  @spec infix_operator([char()]) :: NimbleParsec.t()
  def infix_operator(list_valid_chars) do
    ascii_char(list_valid_chars) |> map(:oper2atom)
  end
end

defmodule Calculator do
  @moduledoc """
  Pending... Documentation for Calculator.

    expr :=  sum (+|-) expr | sum
    sum  :=  factor (*|/) sum | factor
    factor := ( expr ) | num
    num  :=  ...

  """

  import NimbleParsec
  import Calculator.Helpers

  @doc """
  Eval

  ## Examples

      iex> Calculator.eval("1+2*3")
      7.0

  """

  def eval(input) do
    result = parse(input)
    ast = elem(result, 1)
    compiled = flat_ast_postfix(ast)
    run(compiled, [])
  end

  defp run([], [n | []]) do
    n
  end

  defp run([h | t], stack) do
    case h do
      {:push_num, num} -> run(t, [num | stack])
      {:exec2, oper} -> run(t, oper2(stack, oper))
    end
  end

  defp oper2([r | [l | t]], oper) do
    calc = apply(Kernel, oper, [l, r])

    [calc | t]
  end

  # --------

  positive_num =
    integer(min: 1)
    |> optional(
      ignore(ascii_char([?.]))
      |> concat(integer(min: 1))
    )
    |> reduce({Enum, :join, ["."]})
    |> map({Float, :parse, []})
    |> map({Kernel, :elem, [0]})
    |> unwrap_and_tag(:num)

  defp to_negative({:num, n}) do
    {:num, -1 * n}
  end

  num =
    choice([
      ignore(ascii_char([?-]))
      |> concat(positive_num)
      |> map(:to_negative),
      positive_num
    ])

  factor =
    choice([
      ignore(ascii_char([?(]))
      |> parsec(:expr)
      |> ignore(ascii_char([?)])),
      num
    ])

  defp oper2atom(ch_oper) do
    case ch_oper do
      ?+ -> {:oper, :+}
      ?- -> {:oper, :-}
      ?* -> {:oper, :*}
      ?/ -> {:oper, :/}
    end
  end

  defcombinatorp(:infix_oper_mult, infix_operator([?*, ?/]))
  defcombinatorp(:infix_oper_sum, infix_operator([?+, ?-]))

  defcombinatorp(
    :sum,
    choice([
      factor
      |> parsec(:infix_oper_mult)
      |> parsec(:sum)
      |> tag(:fn2),
      factor
    ])
  )

  defcombinatorp(
    :expr,
    choice([
      parsec(:sum)
      |> parsec(:infix_oper_sum)
      |> parsec(:expr)
      |> tag(:fn2),
      parsec(:sum)
    ])
  )

  fexpr = parsec(:expr) |> eos

  defparsecp(:parse, fexpr)

  defp flat_ast_postfix(num: n) do
    [push_num: n]
  end

  defp flat_ast_postfix(fn2: [exp1, {:oper, op}, exp2]) do
    flat_ast_postfix([exp1]) ++ flat_ast_postfix([exp2]) ++ [exec2: op]
  end

  @spec tdd() :: [any()]
  def tdd do
    inputs = [
      "3.141592653592",
      "-1+2+3",
      "-1+2+-3",
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
      "1+2+3",
      "-1+2"
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

# Calculator.tdd()
# IO.inspect(Calculator.eval("3.1415926535"))
# IO.inspect(Calculator.eval("1+2"))
# IO.inspect(Calculator.eval("1*2"))
# IO.inspect(Calculator.eval("1-2"))
# IO.inspect(Calculator.eval("1/2"))
# IO.inspect(Calculator.eval("1+2*3"))
# IO.inspect(Calculator.eval("1+(2+(3*7))"))
