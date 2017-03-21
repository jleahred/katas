module Main where

import Lib
import Control.Parallel.Strategies
import System.IO
-- import Control.Concurrent

main :: IO ()
main = do 
    let (a, b) = paral
    a
    b


paral = runEval $ do
    a <- rpar (out 'a')
    b <- rpar (out '.')
    rseq b
    rseq a
    return (a, b)

paral2 = runEval $ do
    a <- rpar out2
    b <- rpar out2
    rseq a
    rseq b
    return (a, b)


out a = out' a 10000
    where out' a 0 = putChar a
          out' a n = do
            putChar a
            -- hFlush stdout
            -- threadDelay 1000
            out' a (n-1)


out2 = do
    putStrLn "Insert char"
    c <- getChar
    out c