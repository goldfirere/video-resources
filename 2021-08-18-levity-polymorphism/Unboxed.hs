{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedNewtypes, ExplicitForAll,
             KindSignatures, StandaloneKindSignatures, DataKinds, PolyKinds #-}

module Main where

import Data.Kind
import GHC.Exts

data Parity = Even | Odd

instance Num Parity where
  Even + Even = Even
  Even + Odd  = Odd
  Odd  + Even = Odd
  Odd  + Odd  = Even

  p1 - p2 = p1 + p2

  Odd * Odd = Odd
  _   * _   = Even

  negate p = p
  abs    p = p
  signum p = p

  fromInteger p | even p    = Even
                | otherwise = Odd

data ParityInteger = MkPI Parity Integer

piFromInteger :: Integer -> ParityInteger
piFromInteger n | even n    = MkPI Even n
                | otherwise = MkPI Odd n

add :: ParityInteger -> ParityInteger -> ParityInteger
add (MkPI p1 i1) (MkPI p2 i2) = MkPI (p1 + p2) (i1 + i2)

bigPI :: ParityInteger
bigPI = go (piFromInteger 0) [1 .. 100000]
  where
    go :: ParityInteger -> [Integer] -> ParityInteger
    go acc []       = acc
    go acc (i : is) = go (add acc (piFromInteger i)) is

integerFromPI :: ParityInteger -> Integer
integerFromPI (MkPI _ i) = i

type ParityInteger# :: TYPE (TupleRep [LiftedRep, LiftedRep])
newtype ParityInteger# = MkPI# (# Parity, Integer #)

piFromInteger# :: Integer -> ParityInteger#
piFromInteger# n | even n    = MkPI# (# Even, n #)
                 | otherwise = MkPI# (# Odd, n #)

add# :: ParityInteger# -> ParityInteger# -> ParityInteger#
add# (MkPI# (# p1,  i1 #)) (MkPI# (# p2, i2 #)) = MkPI# (# p1 + p2, i1 + i2 #)

bigPI# :: () -> ParityInteger#
bigPI# _ = go (lpFromInteger 0) [1 .. 100000]
  where
    -- go :: ParityInteger# -> [Integer] -> ParityInteger#
    -- go :: forall (r :: RuntimeRep) (a :: TYPE r). LPNum a => a -> [Integer] -> a
    go :: forall (a :: TYPE (TupleRep [LiftedRep, LiftedRep])). LPNum a => a -> [Integer] -> a
    go acc []       = acc
    go acc (i : is) = go (lpPlus acc (lpFromInteger i)) is

integerFromPI# :: ParityInteger# -> Integer
integerFromPI# (MkPI# (# _,  i #)) = i

main :: IO ()
main = print (lpToInteger (bigPI# ()))

class LPNum (a :: TYPE r) where
  lpPlus :: a -> a -> a    -- a :: TYPE r
  lpFromInteger :: Integer -> a
  lpToInteger :: a -> Integer

instance LPNum Integer where
  lpPlus = (+)
  lpFromInteger = fromInteger
  lpToInteger = id

instance LPNum ParityInteger where
  lpPlus = add
  lpFromInteger = piFromInteger
  lpToInteger (MkPI _ i) = i

instance LPNum ParityInteger# where
  lpPlus x y = add# x y
  lpFromInteger = piFromInteger#
  lpToInteger = integerFromPI#
