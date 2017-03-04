module Main where

import System.Random
import Data.List
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle


-- (|>) x f =  f x
(|>) :: a -> (a->b) -> b
(|>) = flip ($)




type VisitInfo = (Int, Int)
type MachInf = (String, [VisitInfo])

machines :: [MachInf]
machines = [
        ("m1",  [( 8, 15), ( 2, 20), ( 2, 30)]),
        ("m2",  [( 7, 20), ( 3, 30), ( 2, 50)]),
        ("m3",  [( 6, 25), ( 4, 35), ( 2, 55)]),
        ("m4",  [( 8, 15), ( 2, 20), ( 2, 30)]),
        ("m5",  [( 8, 12), ( 2, 15), ( 2, 28)]),
        ("m6",  [( 8, 15), ( 2, 20), ( 2, 30)]),
        ("m7",  [( 7, 20), ( 3, 30), ( 2, 50)]),
        ("m8",  [( 6, 25), ( 4, 35), ( 2, 55)]),
        ("m9",  [( 8, 15), ( 2, 20), ( 2, 30)]),
        ("m10", [( 8, 12), ( 2, 15), ( 2, 28)]),
        ("m11", [( 8, 15), ( 2, 20), ( 2, 30)]),
        ("m12", [( 8, 12), ( 2, 15), ( 2, 28)])]





main :: IO ()
main = do
        if not  allMachinesSameVisits  then
          error "Different number of total visits on machines."
        else
          print "Number of visits OK"

        findSolutions  1  1000000  -- niter and estimation value



-- It will generate a new schedule (solution candidate)
-- if better -> print it
-- repeat for ever  (tail recursion)
findSolutions:: Int -> Int -> IO ()
findSolutions   niter
                prev_est
                = do
        schedule <-  mapM rdnVisitsMach machines
        let sum_periods = sumPeriods schedule
        let nw_est = (\(mn, mx) -> mx-mn) $ maxMin sum_periods
        let best_est = min  prev_est  nw_est

        when (best_est /= prev_est)
                -- found better, print it
                (printSolutionCandidate niter best_est sum_periods schedule)

        when (niter == 1000000)
                (print $ "Iteration_____________ " ++ show niter)
        findSolutions  (niter+1)  best_est

printSolutionCandidate:: Show p => Int -> Int -> [Int] -> p -> IO ()
printSolutionCandidate  niter
                        estim
                        sum_periods
                        schedule
                        = do
                                print $ "Iteration " ++ show niter
                                print schedule
                                print sum_periods
                                print estim



--  check configuration. All machines has to have the same quantity of visits
--  you can introduce phony visits with time =0 it you need it
allMachinesSameVisits :: Bool
allMachinesSameVisits   =     mx == mn
        where (mx, mn) = maxMin $ getMachVisits machines
              --  returns a list with number of visit per machine
              getMachVisits lmach = lmach
                                |> map snd
                                |> map (map fst)
                                |> map sum


--  returns the maximun and minim values on a list
maxMin :: Ord a => [a] -> (a, a)
maxMin l = foldl (\(mi, ma) x -> (min mi x, max ma x)) (head l, head l) l




-- get a random visits info for a machine
rdnVisitsMach:: (MonadRandom m) => MachInf -> m (String, [(Int, Int)])
rdnVisitsMach  (mach, visits)  = do 
        cl <- shuffleM $ expandVisitInfo visits
        return (mach, cl)
        where expandVisitInfo visits = foldl (\acc visit ->
                                              acc ++ expandVisit visit)
                                           []
                                           (zip [1..] visits)
              expandVisit (idx, (n, t)) = replicate n (idx, t)


-- get a list with the sum per period (perhaps months) of all machines
sumPeriods:: [(String, [(Int, Int)])] -> [Int]
sumPeriods s =  s
                |> map snd
                |> map (map snd)
                |> transpose
                |> map sum
