module Newtypes where

data Data = MkData Int
data StrictData = MkStrict !Int
newtype Newtype = MkNewtype Int

fData (MkData n) = [1, n]
fStrict (MkStrict n) = [1, n]
fNewtype (MkNewtype n) = [1, n]
