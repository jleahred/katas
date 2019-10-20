defmodule PerimTest do
  
  use ExUnit.Case

  def dotest(n, exp) do
      IO.puts("Testing : #{n}")
      act = Perim.perimeter(n)
      IO.puts("act: #{act}")
      IO.puts("exp: #{exp}")
      assert act == exp
      IO.puts("#")
  end
  
  test "perimeter" do
      dotest(5, 80)
      dotest(7, 216)
      dotest(20, 114624)
      dotest(30, 14098308)
      
  end
end