{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, StandaloneKindSignatures,
             TypeApplications, ScopedTypeVariables, FlexibleContexts,
             AllowAmbiguousTypes #-}

module PickType where

import GHC.TypeLits
import Type.Reflection
import Data.Kind

type PickType :: forall k. Nat -> k
type family PickType n where
  PickType @Type 1 = Int
  PickType @Type 2 = Bool
  PickType @(Type -> Type) 3 = Maybe

printType :: forall (n :: Nat) k. Typeable (PickType @k n) => String
printType = show (typeRep @(PickType @k n))

int :: String
int = printType @1 @Type
