module Main where

import Lib
import Control.Parallel.Strategies


-- time ./sambamba-exe +RTS -N1 -RTS
-- ./sambamba-exe +RTS -N1 -RTS  7,28s user 0,05s system 99% cpu 7,374 total

-- time ./sambamba-exe +RTS -N2 -RTS
-- ./sambamba-exe +RTS -N2 -RTS  10,31s user 5,79s system 181% cpu 8,874 total

-- time ./sambamba-exe +RTS -N4 -RTS
-- ./sambamba-exe +RTS -N4 -RTS  27,18s user 23,06s system 322% cpu 15,569 total


-- time ./sambamba-exe +RTS -N4 -s -RTS
-- ./sambamba-exe +RTS -N4 -RTS  27,18s user 23,06s system 322% cpu 15,569 total



p1 = 986444689 
p2 = 986541307 


main :: IO ()
main = do 
    let (a, b) = paral2
    -- let a = isPrime p1
    -- let b = isPrime p2
    b
    print a


nonpar = (isPrime p1, isPrime p2)

paral2 = runEval $ do
    a <- rpar $ isPrime p1
    -- b <- rpar $ isPrime p2
    b <- rpar printName
    rseq b
    rseq a
    return (a, b)


isPrime n = null [x | x<-[2..n `div` 2], n `mod` x ==0] 
    -- where isqrt = floor . sqrt . fromIntegral


printName = do
    putStrLn "Inser your name"
    name <- getLine
    putStrLn name