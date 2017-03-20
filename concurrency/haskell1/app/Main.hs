module Main where

import Lib
import Control.Parallel.Strategies

main :: IO ()
main = do 
    -- out "a"
    -- out "."
    (a, b) <- paral2
    a
    b


paral = runEval $ do
    a <- rpar (out 'a')
    b <- rpar (out '.')
    rseq b
    rseq a
    return a

paral2 = runEval $ do
    a <- rpar out2
    b <- rpar out2
    rseq a
    rseq b
    return (a, b)


out a = out' a 1000
    where out' a 0 = putChar a
          out' a n = do
            putChar a
            out' a (n-1)


out2 = do
    c <- getChar
    out c