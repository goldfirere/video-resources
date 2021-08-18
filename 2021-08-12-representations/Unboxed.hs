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

identity :: forall (a :: Type). a -> a
identity x = x

type ParityInteger# :: TYPE (TupleRep [LiftedRep, LiftedRep])
newtype ParityInteger# = MkPI# (# Parity, Integer #)

piFromInteger# :: Integer -> ParityInteger#
piFromInteger# n | even n    = MkPI# (# Even, n #)
                 | otherwise = MkPI# (# Odd, n #)

add# :: ParityInteger# -> ParityInteger# -> ParityInteger#
add# (MkPI# (# p1,  i1 #)) (MkPI# (# p2, i2 #)) = MkPI# (# p1 + p2, i1 + i2 #)

bigPI# :: () -> ParityInteger#
bigPI# _ = go (piFromInteger# 0) [1 .. 100000]
  where
    go :: ParityInteger# -> [Integer] -> ParityInteger#
    go acc []       = acc
    go acc (i : is) = go (add# acc (piFromInteger# i)) is

integerFromPI# :: ParityInteger# -> Integer
integerFromPI# (MkPI# (# _,  i #)) = i

main :: IO ()
main = print (integerFromPI# (bigPI# ()))
