defmodule Fix.Static.ExecTypes do
  @external_resource Path.join(__DIR__, "exec_types.txt")
  @code_names File.stream!(@external_resource)
              |> Stream.map(fn line ->
                line
                |> String.replace("\n", "")
                |> String.split(" ", parts: 2)
                |> List.to_tuple()
              end)

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

  def get_name(unknown) do
    if unknown == "" do
      ""
    else
      "unknown: #{inspect(unknown)}"
    end
  end
end
