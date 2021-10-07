{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module NatUnsafe (
    Nat(..)
  , unsafeAssumeSucc
  , unsafeAssumeEqual
  ) where

import NatSafe

import Data.Type.Equality
import Data.Kind
import Unsafe.Coerce

-- | Assume an arbitrary equality among 'Nat's
unsafeAssumeEqual :: forall n m r. (n ~ m => r) -> r
unsafeAssumeEqual k = case unsafeCoerce @(() :~: ()) @(n :~: m) Refl of
  Refl -> k

-- | A GADT representing that a 'Nat' is a successor of another
type IsSucc :: Nat -> Type
data IsSucc n where
  MkIsSucc :: IsSucc (Succ n')

-- | Assume an arbitrary 'Nat' is actually a 'Succ' node
unsafeAssumeSucc :: forall n r. (forall m. (n ~ Succ m) => r) -> r
unsafeAssumeSucc k = case unsafeCoerce @(IsSucc (Succ Zero)) @(IsSucc n) MkIsSucc of
  mk@MkIsSucc -> case mk of (_ :: IsSucc (Succ m)) -> k @m