module Main where

import System.Random 
import Data.List
--import Control.Monad



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

        seed <- newStdGen
        let (schedule, nwseed) = genSchedule machines seed
        let sum_periods = sumPeriods schedule
        let estimation = (\(mn, mx) -> mx-mn) $ maxMin sum_periods
        print schedule
        print sum_periods
        print estimation

        findSolutions  1 schedule sum_periods estimation  nwseed




findSolutions niter prev_schedule prev_sum_periods prev_estimation seed = do
        let (schedule, nwseed) = genSchedule machines seed
        let sum_periods = sumPeriods schedule
        let estimation = (\(mn, mx) -> mx-mn) $ maxMin sum_periods

        if prev_estimation > estimation then do
                print $ "Iteration " ++ show niter
                print schedule
                print sum_periods
                print estimation
                findSolutions  (niter+1) schedule sum_periods estimation  nwseed
        else
                findSolutions  (niter+1) prev_schedule prev_sum_periods prev_estimation  nwseed
        






allMachinesSameVisits ml = mx == mn
        where (mx, mn) = maxMin $ getMachVisits machines
              --  returns a list with number of visit per machine
              getMachVisits lmach = lmach 
                                |> map snd
                                |> map (map fst)
                                |> map sum


--  returns the maximun and minim values on a list
maxMin :: Ord a => [a] -> (a, a)
maxMin l = foldl (\(mi, ma) x -> (min mi x, max ma x)) (head l, head l) l




clutterList::  Ord a =>  StdGen -> [a] -> ([a], StdGen)
clutterList seed xs =  do
        let (zrl, newseed) = mapRnd  (\ x seed -> (fst $ next seed, x)) xs seed
        (zrl |> sort |> map snd, newseed)
        where mapRnd f xs seed = 
                foldl (\(ys, nwseed) y -> ((f y nwseed):ys, snd $ next nwseed))  ([], seed)  xs  





rdnVisitsMach :: StdGen -> MachInf -> ((String, [(Int, Int)]), StdGen)
rdnVisitsMach seed (m, visits) = do
        let (cl, nwseed) = clutterList seed $ expandVisitInfo visits
        ((m, cl), nwseed)
        where expandVisitInfo visits = foldl (\acc visit -> 
                                              acc ++ expandVisit visit) 
                                           []
                                           (zip [1..] visits)
              expandVisit (idx, (n, t)) = replicate n (idx, t)



genSchedule :: [MachInf] -> StdGen -> ([(String, [(Int, Int)])], StdGen)
genSchedule [] seed = ([], seed)
genSchedule (m:ms) seed = do
        let (rvm, nwseed) = rdnVisitsMach seed m
        let (msGenSche, nwseed') = genSchedule ms nwseed
        (rvm:msGenSche, nwseed')





sumPeriods s =  s 
                |> map snd
                |> map (map snd)
                |> transpose
                |> map sum


