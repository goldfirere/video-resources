{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module SNatSafe (
  SNat(..),
  snatToInt,
  withSNat
  ) where

import Data.Kind
import NatSafe
import Numeric.Natural

-- | A singleton for 'Nat's
type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- | Convert a 'SNat' to an 'Int'
snatToInt :: SNat n -> Int
snatToInt SZero = 0
snatToInt (SSucc n) = 1 + snatToInt n

-- | Convert a 'Natural' to an 'SNat', in continuation-passing
-- style.
withSNat :: Natural -> (forall n. SNat n -> r) -> r
withSNat 0 k = k SZero
withSNat n k = withSNat (n-1) $ \ n' ->
               k (SSucc n')