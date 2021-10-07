{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module VecSafe (
    Vec(..)
  , vecNth
  , vecOfFins
  , buildVec
  ) where

import NatSafe
import Data.Kind
import SNatSafe
import FinSafe

-- | A length-indexed vector
type Vec :: Nat -> Type -> Type
data Vec n a where
  VNil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)

-- | Index into the vector with a 'Fin'
vecNth :: Vec n a -> Fin n -> a
vecNth vec fin = case (fin, vec) of
  (FZero, x :> _)        -> x
  (FSucc index, _ :> xs) -> vecNth xs index

-- | Create a vector containing the @n@ possible values
-- of type @'Fin' n@.
vecOfFins :: SNat n -> Vec n (Fin n)
vecOfFins SZero     = VNil
vecOfFins (SSucc n) = FZero :> fmap FSucc (vecOfFins n)

-- | Create a vector of length @n@, where each element
-- is the result of a function taking a @'Fin' n@.
buildVec :: SNat n -> (Fin n -> a) -> Vec n a
buildVec size mk_elem = fmap mk_elem (vecOfFins size)
