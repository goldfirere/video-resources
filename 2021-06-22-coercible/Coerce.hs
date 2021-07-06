{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving,
             RoleAnnotations, GADTs #-}

{-# OPTIONS_GHC -Wall #-}

module Coerce where

import Data.Ord
import Data.Coerce

newtype Age = MkAge Int
  deriving newtype Eq
  deriving Ord via (Down Int)

newtype Age2 = MkAge2 Age

toAge :: Int -> Age
toAge n | 0 <= n
        , n <= 120 = coerce n
        | otherwise = error "bad Age"

fromAge :: Age -> Int
fromAge = coerce

toAges :: [Int] -> [Age]
toAges = coerce

frob :: Either Int Age2 -> Either Age2 Int
frob = coerce

data Set a = MkSet [a]   -- INVARIANT: list is in non-decreasing order according to Ord
type role Set nominal

minElement :: Set a -> a
minElement (MkSet (x:_)) = x
minElement _ = error "no elements"
{-
changeSet :: Set Age -> Set Int
changeSet = coerce
-}
data Phant a = MkPhant
type role Phant phantom

changePhant :: Phant Int -> Phant Bool
changePhant = coerce

data G a where
  MkG1 :: G Age
  MkG2 :: G Int

f :: G Age -> Bool
f MkG1 = True

changeG :: G Int -> G Age
changeG = coerce    -- what about f (changeG MkG2)
