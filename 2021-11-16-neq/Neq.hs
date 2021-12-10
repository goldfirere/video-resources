module Neq where

import Prelude hiding ( Eq(..) )

{- TODAY:
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x == y = not (x /= y)
  x /= y = not (x == y)
-}

-- TOMORROW:

class Eq a where
  (==) :: a -> a -> Bool

(/=) :: Eq a => a -> a -> Bool
x /= y = not (x == y)