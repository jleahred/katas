module Lib
    ( Idadi
    , sufuri
    , ijayo
    , moja
    , uliopita
    , jumla
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