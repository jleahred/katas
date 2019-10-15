# defmodule ProdFib do
#   def product_fib(n) do
#     fib_find_prod({1, 1}, n)
#   end

#   defp fib_find_prod({n0, n1}, n) do
#     prod = n0 * n1

#     cond do
#       prod >= n -> [n0, n1, prod == n]
#       true -> fib_find_prod({n1, n0 + n1}, n)
#     end
#   end

#   # defp fib_find_prod({n0, n1}, n) when n0 * n1 >= n, do: [n0, n1, n0 * n1 == n]
#   # defp fib_find_prod({n0, n1}, n), do: fib_find_prod({n1, n0 + n1}, n)

#   # def fib_find_prod({n0, n1}, n, find_prod) do
#   #   next = n0 + n1
#   #   prod = n1 * next

#   #   if prod > find_prod do
#   #     {n1, next, false}
#   #   else
#   #     case prod == find_prod do
#   #       true -> {n1, next, true}
#   #       false -> fib_find_prod({n1, next}, n + 1, find_prod)
#   #     end
#   #   end
#   # end
# end

# defmodule ProdFib do
#   def product_fib(n) do
#     {a,b} = Stream.iterate({1, 1}, fn {a, b} -> {b, a + b} end)
#     |> Enum.take_while(|{a,b}| a*b <= n)
#     |> Enum.take(-1)

#     if a*b == n do
#       {a, b, true}
#     else
#       {b, a+b, false}
#     end
#   end

defmodule ProdFib do
  def product_fib(n) do
    Stream.iterate({1, 1}, fn {a, b} -> {b, a + b} end)
    |> Enum.find(fn {a, b} -> a * b >= n end)
    |> (fn {a, b} -> {a, b, a * b == n} end).()
  end
end
