defmodule ElistTest do
  use ExUnit.Case
  doctest Elist

  test "greets the world" do
    assert Elist.hello() == :world
  end
end
