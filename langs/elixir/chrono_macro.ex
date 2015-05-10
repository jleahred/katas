 
defmodule  Chrono do


	def  as_microseconds  date  do
		{megas, sec, micros} = date
		megas*1000000 + sec + micros
	end


	def tc(module, f, params)  do
		init_ = as_microseconds :erlang.now 
		
		result = :erlang.apply(module, f, params)
		
		end_ = as_microseconds :erlang.now 
		{end_ - init_, result}
	end

	def tc(lambda)  do
		init_ = as_microseconds :erlang.now 
		
		result = lambda.()
		
		end_ = as_microseconds :erlang.now 
		{end_ - init_, result}
	end

	
	defmacro  chrono [do: block] do
		quote do
			init_ = Chrono.as_microseconds :erlang.now 
			result = unquote block
			end_ = Chrono.as_microseconds :erlang.now 
			{end_ - init_, result}
		end
	end
	
	defmacro  chrono message, [do: block] do
		quote do
			init_ = Chrono.as_microseconds :erlang.now 
			result = unquote block
			end_ = Chrono.as_microseconds :erlang.now 
			IO.puts "#{unquote(message)} requiered #{end_ - init_}"
			result
		end
	end

end





defmodule Pr  do

import Chrono


def factorial n  do
	1..n  |>  Enum.reduce(1, fn x, acc -> x*acc end)
end

def test  do

	IO.puts inspect Chrono.tc(Pr, :factorial, [1000])

	IO.puts inspect Chrono.tc(fn -> factorial(1000) end)

	chrono do: factorial(1000)

	chrono do
		factorial(1000)
		factorial(1000)
	end

	chrono "two factorials" do
		factorial(1000)
		factorial(1000)
	end

end

def test2 do
	a = if true, do: 2; -;3
	IO.puts a
end

end
