module Lib
    ( Idadi
    , sufuri
    , ijayo
    , moja
    , uliopita
    , jumla
    , bidhaa
    , jumla2
    , bidhaa2
    , (.+.)
    , (.*.)
    , int2idadi
    , idadi2int
    ) where



type Idadi = [()]

sufuri :: Idadi
sufuri = []


ijayo :: Idadi -> Idadi
ijayo  i  =  () : i


moja :: Idadi
moja = ijayo sufuri

uliopita :: Idadi -> Idadi
uliopita (():i) = i

jumla :: Idadi -> Idadi -> Idadi
jumla i1 [] = i1
jumla i1 i2 = jumla (ijayo i1) (uliopita i2)

jumla2 :: Idadi -> Idadi -> Idadi
jumla2 i1 i2 = foldl (\acc _ -> ():acc) i1 i2


bidhaa :: Idadi -> Idadi -> Idadi
bidhaa i1 i2 = bidhaa' [] i1 i2
    where   bidhaa' acc i1 [] = acc
            bidhaa' acc i1 i2 = bidhaa' (jumla acc i1) i1 (uliopita i2)


bidhaa2 :: Idadi -> Idadi -> Idadi
bidhaa2 i1 i2 = foldl (\acc _ -> jumla acc i1) [] i2

(.+.) = jumla2
(.*.) = bidhaa2

int2idadi :: Int -> Idadi
int2idadi int = int2idadi' int []
    where int2idadi' 0 acc = acc
          int2idadi' int acc = int2idadi' (int-1) (():acc)

idadi2int :: Idadi -> Int
idadi2int i = foldl (\acc _-> acc+1) 0 i