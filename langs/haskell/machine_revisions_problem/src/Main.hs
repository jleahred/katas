module Main where

import System.Random 
import Data.List
--import Control.Monad



(|>) x f =  f x




main :: IO ()
main = do
        if not $ allMachinesSameVisits machines  then
          error "Different number of total visits on machines."
        else
          print "Number of visits OK"

        s0 <- newStdGen
        let (r, s1) = next s0
        let (r, s2) = next s1
        let schedule = generateSchedule s2
        let sum_periods = sumPeriods schedule
        let estimation = (\(mn, mx) -> mx-mn) $ maxMin sum_periods
        print schedule
        print sum_periods
        print estimation

        -- findSolutions  schedule sum_periods estimation




findSolutions prev_schedule prev_sum_periods prev_estimation = do
        seed <- newStdGen
        let schedul = generateSchedule seed
        let sum_periods = sumPeriods schedul
        let estimation = (\(mn, mx) -> mx-mn) $ maxMin sum_periods

        if prev_estimation > estimation then do
                print schedul
                print sum_periods
                print estimation
                findSolutions  schedul sum_periods estimation
        else
                findSolutions  prev_schedule prev_sum_periods prev_estimation
        







type VisitInfo = (Int, Int)
type MachineInfo = (String, [VisitInfo])

machines :: [MachineInfo]
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





randomizeList seed l = l
                  |> zip (rndList seed)
                  |> sort
                  |> map snd



rndList  = rnds
        where rnds = unfoldr (Just . next) 




randomVisitMachine :: StdGen -> MachineInfo -> (String, [(Int, Int)])
randomVisitMachine seed (m, visits) = 
        (m, randomizeList seed $ expandVisitInfo visits)
        where expandVisitInfo visits = foldl (\acc visit -> 
                                              acc ++ expandVisit visit) 
                                           []
                                           (zip [1..] visits)
              expandVisit (idx, (n, t)) = replicate n (idx, t)


generateSchedule seed =  map (randomVisitMachine seed)  machines

sumPeriods s =  s 
                |> map snd
                |> map (map snd)
                |> transpose
                |> map sum

