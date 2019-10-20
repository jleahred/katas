defmodule TestShepherd do
  use ExUnit.Case
  import Shepherd, only: [count_sheeps: 1]

  test "work for some examples" do
    assert count_sheeps([]) == 0
    assert count_sheeps([true]) == 1
    assert count_sheeps([true, false]) == 1
    assert count_sheeps([false, false]) == 0

    assert count_sheeps([
             true,
             true,
             true,
             false,
             true,
             true,
             true,
             true,
             true,
             false,
             true,
             false,
             true,
             false,
             false,
             true
           ]) == 11
  end
end
