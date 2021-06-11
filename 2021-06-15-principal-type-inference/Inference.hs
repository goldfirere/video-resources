{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances,
             NoMonomorphismRestriction #-}

module Inference where

frobble x = if x == 3 then show (x < 5) else show [x, x+1]
-- Num a
-- Eq a
-- Ord a
-- Show [a]

y :: Integer
y = 4

result = frobble y

useFrobble :: (Num a, Ord a, Show [a]) => a -> String
useFrobble x = frobble x

class C a
class D a where
  deflt :: a

instance C a => D a where
  deflt = undefined

-- wurble :: C a => a
wurble = deflt

foo :: D a => a
foo = wurble
