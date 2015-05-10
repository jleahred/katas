defmodule DivForever do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
      # worker(DivForever.Worker, [arg1, arg2, arg3])
      worker(Task, [fn -> DivForever.loop end])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DivForever.Supervisor]
    Supervisor.start_link(children, opts)
  end


  def loop2 do
    num = IO.gets("Write a number:")
          |> String.strip
          |> String.to_integer

    IO.puts "1_000_000 / #num == #{1_000_000 / num}"
    loop
  end
end
