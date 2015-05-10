defmodule  Collazt  do

    def biggest_till   n  do
        cache = :ets.new(:cache, [:set, :public, {:write_concurrency, true}])
        start = :erlang.now

        parent = self

        parts = 100  # number of process
        step = max(1, div(n, parts))
        steps = div(n,step)

        1..steps
            |> Enum.each &(spawn(fn -> (biggets_interval (&1-1)*step+1, &1 *step, cache, parent) end))


        result = loop_get_best_result  0, steps,  {0, 0}

        finish = :erlang.now
        IO.puts "it took... #{(:timer.now_diff finish, start)/1000000} seconds"

        IO.puts "cache size... #{:ets.info(cache, :size)}"
        IO.puts "#{inspect(result)}"
        :ets.delete(cache)
    end


    # get best result from all process
    defp  (loop_get_best_result  steps, steps, {r_num, r_counter}),  do:   {r_num, r_counter}

    defp  loop_get_best_result  counter, steps, {r_num, r_counter}  do
        receive do
            {:result, {rnum, rcounter}}  ->
                best = (if r_counter < rcounter,  do:   {rnum, rcounter},
                        else:                           {r_num, r_counter})
                loop_get_best_result  counter+1, steps, best
            _     ->   IO.puts "ERROR"
        end
    end


    defp  biggets_interval  first, till, cache, parent   do
        result = first..till
                    |> Stream.map(&({&1, (serie_size 1, cache, &1)}))
                    |> Enum.max_by &(elem &1, 1)
        send parent, { :result, result }
    end


    defp  (serie_size  acc,  _ , 1),  do:   acc

    defp   (serie_size   acc, cache, n)     do
        case   :ets.lookup(cache, n)   do
            []  ->
                next =   if  (rem n, 2) == 0,   do:   (div n,2),
                        else:                         (3*n + 1)
                result = serie_size  acc+1,  cache, next

                (:ets.insert  cache, {n, result-acc})
                result

            [{_, s}] ->      s + acc
        end

    end

end