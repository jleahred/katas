module Main where

import Lib

import Control.Concurrent
import System.IO

p1 = 986444689 
p2 = 986541307 


main = do
  forkIO $ do printR '.' 1000
  forkIO $ do printR '-' 1000
  let r = isPrime p1
  print r
  print "end"
  


isPrime  = isPrime' 
    where isPrime' n = null [x | x<-[2..n `div` 3], n `mod` x==0] 
    -- where isqrt = floor . sqrt . fromIntegral


printName = do
    putStrLn "Inser your name"
    name <- getLine
    putStrLn name

printR  _  0 = return ()
printR  ch n = do
    putChar ch
    hFlush stdout
    -- _ <- getChar
    threadDelay 1000000
    printR ch  (n-1)