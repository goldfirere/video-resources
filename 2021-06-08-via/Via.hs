{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}

module Via where

import Data.Ord

newtype Supremum a = MkS a
  deriving (Eq, Ord, Bounded)

instance Ord a => Semigroup (Supremum a) where
  (<>) = max
instance (Ord a, Bounded a) => Monoid (Supremum a) where
  mempty = minBound

data DangerLevel
  = AllOK
  | NotGreat
  | UhOh
  | RealProblemHere
  | Catastrophe
  deriving stock (Eq, Ord, Bounded, Show)
  deriving (Semigroup, Monoid) via (Supremum DangerLevel)

data MovieRating
  = G
  | PG
  | PG13
  | R
  deriving stock (Eq, Ord, Bounded)
  deriving (Semigroup, Monoid) via (Supremum MovieRating)

newtype FloodLevel = MkFL Int
  deriving (Eq, Ord, Bounded) via Int
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Supremum (Down FloodLevel))
