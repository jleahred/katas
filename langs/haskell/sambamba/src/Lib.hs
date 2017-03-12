module Lib
    ( Idadi
    , sufuri
    , ijayo
    , moja
    , jumla
    ) where



type Idadi = [()]

sufuri :: Idadi
sufuri = []


ijayo :: Idadi -> Idadi
ijayo  i  =  () : i


moja :: Idadi
moja = ijayo sufuri



jumla :: Idadi -> Idadi -> Idadi
jumla i1 i2 = i1 ++ i2