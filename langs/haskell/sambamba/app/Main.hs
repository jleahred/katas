module Main where

import Lib

main :: IO ()
main = do
    let v1 = [(),(),(),(),()]
    let v2 = [(),(),(),(),(),(),()]
    print $  v1 .*. v2
    let v3 = int2idadi 459
    let v4 = int2idadi 674
    print $ v3 .*. v4
    print $ idadi2int((int2idadi 40987) .*. (int2idadi 1908)) == 40987*1908
