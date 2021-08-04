{-# LANGUAGE StandaloneKindSignatures, ConstraintKinds, FunctionalDependencies,
             FlexibleInstances, DataKinds, ScopedTypeVariables #-}

module Kinds where

import Data.Kind
import qualified Data.Set as Set
import GHC.TypeLits

s1 :: Set.Set Int
s1 = Set.fromList [2,4,2,3]

s2 :: Set.Set Int
s2 = Set.map (+1) s1

s3 :: Set.Set Int
s3 = fmapC (+1) s1


type FunctorC :: (Type -> Constraint) -> (Type -> Constraint) -> (Type -> Type) -> Constraint
class FunctorC c1 c2 f | f -> c1 c2 where
  fmapC :: (c1 a, c2 b) => (a -> b) -> f a -> f b

type Always :: Type -> Constraint
class Always a
instance Always a

instance FunctorC Always Ord Set.Set where
  fmapC = Set.map

------------------------------------

type IntMod :: Nat -> Type
newtype IntMod n = MkIM Integer
  deriving Show

instance KnownNat n => Num (IntMod n) where
  i@(MkIM a) + MkIM b = MkIM ((a + b) `mod` natVal i)
  fromInteger n = MkIM (n `mod` natVal (undefined :: IntMod n))
