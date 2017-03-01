module Main where

import System.Random
import Data.List
import Control.Monad



-- (|>) x f =  f x
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
        if not $ allMachinesSameVisits machines  then
          error "Different number of total visits on machines."
        else
          print "Number of visits OK"

        gen <- newStdGen
        findSolutions  1  1000000  gen



-- It will generate a new schedule (solution candidate)
-- if better -> print it
-- repeat for ever  (tail recursion)
findSolutions:: Int -> Int -> StdGen -> IO ()
findSolutions   niter
                prev_estimation seed
                = do
        let (schedule, nwseed) = genSchedule machines seed
        let sum_periods = sumPeriods schedule
        let nw_estimation = (\(mn, mx) -> mx-mn) $ maxMin sum_periods
        let best_estimation = min  prev_estimation  nw_estimation

        when (best_estimation /= prev_estimation)
                -- found better, print it
                (printSolutionCandidate niter best_estimation sum_periods schedule)

        findSolutions  (niter+1)  best_estimation  nwseed


printSolutionCandidate:: Show b => Int -> Int -> [Int] -> b -> IO ()
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
allMachinesSameVisits  ml  =     mx == mn
        where (mx, mn) = maxMin $ getMachVisits machines
              --  returns a list with number of visit per machine
              getMachVisits lmach = lmach
                                |> map snd
                                |> map (map fst)
                                |> map sum


--  returns the maximun and minim values on a list
maxMin :: Ord a => [a] -> (a, a)
maxMin l = foldl (\(mi, ma) x -> (min mi x, max ma x)) (head l, head l) l



-- given a list, clutter it (randomize)
clutterList::  Ord a =>  StdGen -> [a] -> ([a], StdGen)
clutterList  seed  xs =  do
        let (zrl, newseed) = mapRnd  (\ x seed -> (fst $ next seed, x)) xs seed
        (zrl |> sort |> map snd, newseed)
        where mapRnd f xs seed =
                foldl (\(ys, nwseed) y -> ((f y nwseed):ys, snd $ next nwseed))  ([], seed)  xs




-- get a random visits info for a machine
rdnVisitsMach :: MachInf -> StdGen -> ((String, [(Int, Int)]), StdGen)
rdnVisitsMach  (m, visits)  seed = do
        let (cl, nwseed) = clutterList seed $ expandVisitInfo visits
        ((m, cl), nwseed)
        where expandVisitInfo visits = foldl (\acc visit ->
                                              acc ++ expandVisit visit)
                                           []
                                           (zip [1..] visits)
              expandVisit (idx, (n, t)) = replicate n (idx, t)


-- generate a random Schedule for all machines
genSchedule :: [MachInf] -> StdGen -> ([(String, [(Int, Int)])], StdGen)
genSchedule [] seed = ([], seed)
genSchedule (m:ms) seed = do
        let (rvm, nwseed) = rdnVisitsMach  m  seed
        let (msSchedules, nwseed') = genSchedule ms nwseed
        (rvm:msSchedules, nwseed')



-- get a list with the sum per period (perhaps months) of all machines
sumPeriods:: [(String, [(Int, Int)])] -> [Int]
sumPeriods s =  s
                |> map snd
                |> map (map snd)
                |> transpose
                |> map sum
