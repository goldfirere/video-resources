module Use where

import Data.Coerce
import Set

fromList :: Ord a => [a] -> Set a
fromList = foldr (union . unit) empty

newtype Parity = P Integer
  deriving Show

instance Eq Parity where
  P a == P b = even a == even b

instance Ord Parity where
  P a `compare` P b = even a `compare` even b
