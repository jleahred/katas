defmodule Fix.Static.Clients do
  @external_resource Path.join(__DIR__, "clients.txt")
  @clients File.stream!(@external_resource)
           |> Enum.map(&String.trim/1)
           |> Enum.into([])

  def all() do
    @clients
  end
end
