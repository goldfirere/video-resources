module A (X(..), sneaky) where

import B

data X = MkX1 Int
       | MkX2 Y

sneaky :: Int -> X -> X
sneaky _ = id