{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SNatUnsafe (
  SNat(SZero, SSucc),
  snatToInt,
  withSNat
  ) where

import Data.Kind
import NatUnsafe
import Numeric.Natural

type SNat :: Nat -> Type
newtype SNat n = UnsafeMkSNat { snatToInt :: Int }

type MatchSZero :: Nat -> Type
data MatchSZero n where
  MSZYes :: MatchSZero Zero
  MSZNo  :: MatchSZero n

matchSZero :: forall n. SNat n -> MatchSZero n
matchSZero (UnsafeMkSNat 0) = unsafeAssumeEqual @n @Zero $ MSZYes
matchSZero _                = MSZNo

pattern SZero :: forall n. () => (n ~ Zero) => SNat n
pattern SZero <- (matchSZero -> MSZYes)

type MatchSSucc :: Nat -> Type
data MatchSSucc n where
  MSSYes :: SNat n -> MatchSSucc (Succ n)
  MSSNo  :: MatchSSucc n

matchSSucc :: forall n. SNat n -> MatchSSucc n
matchSSucc (UnsafeMkSNat n) | n > 0     = unsafeAssumeSucc @n $ MSSYes (UnsafeMkSNat (n-1))
                            | otherwise = MSSNo

pattern SSucc :: forall n. () => forall n'. (n ~ Succ n') => SNat n' -> SNat n
pattern SSucc n <- (matchSSucc -> MSSYes n)

{-# COMPLETE SZero, SSucc #-}

withSNat :: Natural -> (forall n. SNat n -> r) -> r
withSNat n k = k (UnsafeMkSNat (fromIntegral n))