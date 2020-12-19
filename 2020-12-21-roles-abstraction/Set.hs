module Set
  ( Set, empty, unit, union, inSet, delete )
  where

newtype Set a   -- kept abstract
  = Elements [a]
  -- INVARIANTS:
  --   1) in increasing order according to `a`'s Ord instance
  --   2) no duplicates
  deriving Show   -- for debugging only

empty :: Set a
empty = Elements []

unit :: a -> Set a
unit x = Elements [x]

union :: Ord a => Set a -> Set a -> Set a
union (Elements xs) (Elements ys) = Elements (merge xs ys)
  where
    merge [] bs = bs
    merge as [] = as
    merge all_as@(a:as) all_bs@(b:bs) = case compare a b of
      LT -> a : merge as all_bs
      EQ -> merge as all_bs    -- don't prepend 'a'; it might have *another* dup
      GT -> b : merge all_as bs

inSet :: Ord a => a -> Set a -> Bool
inSet x (Elements els) = check x els
  where
    check _ [] = False
    check x (y:ys) = case compare x y of
      LT -> False
      EQ -> True
      GT -> check x ys

delete :: Ord a => Set a -> a -> Set a
delete (Elements els) x = Elements (del els x)
  where
    del []            _ = []
    del all_ys@(y:ys) x = case compare x y of
      LT -> all_ys
      EQ -> ys
      GT -> y : del ys x
