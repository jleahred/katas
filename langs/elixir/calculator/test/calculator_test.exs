defmodule CalculatorTest do
  use ExUnit.Case
  doctest Calculator

  test "greets the world" do
    assert Calculator.hello() == :world
  end
end
