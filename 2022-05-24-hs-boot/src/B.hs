module B (Y(..)) where

import {-# SOURCE #-} A

data Y = MkY1 Bool
       | MkY2 X

f :: Y -> Y
f (MkY1 True) = MkY1 False
f (MkY2 x) = MkY2 (sneaky 5 x)