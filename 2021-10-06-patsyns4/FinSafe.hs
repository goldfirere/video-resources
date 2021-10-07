{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module FinSafe (
  Fin(..),
  finToInt, intToFin, unsafeIntToFin,
  freshFin
  ) where

import NatSafe

import SNatSafe

import Data.Kind
import Data.Maybe (fromJust)

-- | Natural numbers less than some bound
type Fin :: Nat -> Type
data Fin n where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)

instance Show (Fin n) where
  show = show . finToInt

-- | Convert a 'Fin' to an 'Int'
finToInt :: Fin n -> Int
finToInt FZero = 0
finToInt (FSucc n) = 1 + finToInt n

-- | Convert an 'Int' to a 'Fin'. Returns 'Nothing' if the
-- desired 'Fin' cannot represent the 'Int'
intToFin :: SNat n    -- ^ The desired index of the 'Fin'
         -> Int -> Maybe (Fin n)
intToFin SZero _ = Nothing
intToFin (SSucc _) 0 = Just FZero
intToFin (SSucc n) f = fmap FSucc (intToFin n (f-1))

-- | Convert an 'Int' to a 'Fin', where the caller must
-- guarantee that the conversion succeeds.
unsafeIntToFin :: SNat n  -- ^ The desired index of the 'Fin'
               -> Int -> Fin n
unsafeIntToFin n f = fromJust $ intToFin n f

-- | Reallocate a 'Fin', destroying any sharing. (This is
-- for demonstration purposes only.)
freshFin :: Fin n -> Fin n
freshFin FZero = FZero
freshFin (FSucc n) = FSucc (freshFin n)