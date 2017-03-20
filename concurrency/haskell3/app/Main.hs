module Main where

import Lib
import Control.Parallel.Strategies

main :: IO ()
main = do 
    let (a, b) = paral2
    a
    b


paral2 = runEval $ do
    a <- rpar $ out '.'
    b <- rpar out2
    rseq b
    rseq a
    return (a, b)


out a = out' a 1000
    where out' a 0 = putChar a
          out' a n = do
            putChar a
            out' a (n-1)


out2 = do
    putStrLn "insert char.............   "
    c <- getChar
    out c