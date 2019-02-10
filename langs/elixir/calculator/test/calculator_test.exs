defmodule CalculatorTest do
  use ExUnit.Case
  doctest Calculator

  test "operations" do
    assert Calculator.eval("3.1415926535") === 3.1415926535
    assert Calculator.eval("1+2") === 3.0
    assert Calculator.eval("1*2") === 2.0
    assert Calculator.eval("1-2") === -1.0
    assert Calculator.eval("1/2") === 0.5
    assert Calculator.eval("1+2*3") === 7.0
    assert Calculator.eval("1+(2+(3*7))") === 24.0
  end
end
