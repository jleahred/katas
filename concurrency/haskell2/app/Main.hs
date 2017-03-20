module Main where

import Lib
import Control.Parallel.Strategies

-- const BIG_PRIME: u64 = 15485867;

main :: IO ()
main = do 
    (a, b) <- paral2
    print a
    print b



paral2 = runEval $ do
    a <- rpar isPrime 15485867
    a <- rpar isPrime 15485869
    rseq a
    rseq b
    return (a, b)


isPrime n = isPrime' n False
    where isPrime' 