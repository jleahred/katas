defmodule Fix.Static.MsgTypes do
  @moduledoc """
  Functions to convert from msg_type key to msg_name  string -> string
  Generated from msg_types.txt
  """

  @external_resource Path.join(__DIR__, "msg_types.txt")
  @code_names File.stream!(@external_resource)
              |> Stream.map(fn line -> List.to_tuple(String.split(line)) end)

  @all_codes @code_names
             |> Enum.reverse()
             |> Enum.reduce([], fn {c, _n}, acc -> [c | acc] end)

  def list_all_codes() do
    @all_codes
  end

  for {code, name} <- @code_names do
    def get_name(unquote(code)) do
      unquote(name)
    end
  end

  def get_name(_unknown) do
    "unknown"
  end
end
