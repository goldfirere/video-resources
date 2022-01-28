{-# LANGUAGE FlexibleContexts, DerivingVia, UndecidableInstances, TypeFamilies,
             InstanceSigs, ScopedTypeVariables #-}

module GenericLoopy where

import GHC.Generics
import Data.Functor.Const
import qualified Data.Monoid as M
import Data.Coerce

-- This stuff mostly ripped from newer GHC.Generics:
newtype Generically a = Generically a
  deriving stock Show

-- | @since 4.17.0.0
instance (Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
  (<>) :: Generically a -> Generically a -> Generically a
  Generically a <> Generically b = Generically (to (from a <> from b :: Rep a ()))

-- | @since 4.17.0.0
instance (Semigroup a, Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
  mempty :: Generically a
  mempty = Generically (to (mempty :: Rep a ()))

  mappend :: Generically a -> Generically a -> Generically a
  mappend = (<>)

-- Then we build the trap by making type N such that Rep T () = Generically T (almost)
newtype N = N Int
  deriving (Semigroup, Monoid) via M.Sum Int
  deriving stock Show

instance Generic N where
  type Rep N = Const (Generically N)
  from = coerce
  to = coerce

-- Then set the trap:
useSuper :: Monoid a => a -> a
useSuper x = x <> x

-- And spring it:
tryMe = useSuper (Generically (N 1))
