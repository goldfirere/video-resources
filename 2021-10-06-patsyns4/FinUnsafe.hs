{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
module FinUnsafe (
  Fin(FZero, FSucc),
  finToInt, intToFin, unsafeIntToFin,
  freshFin
  ) where

import NatUnsafe

import SNatUnsafe

import Data.Kind
import Data.Maybe (fromJust)

type Fin :: Nat -> Type
newtype Fin n = UnsafeMkFin { finToInt :: Int }
type role Fin nominal

instance Show (Fin n) where
  show = show . finToInt

type MatchFZero :: Nat -> Type
data MatchFZero n where
  MFZYes :: MatchFZero (Succ n)
  MFZNo  :: MatchFZero n

matchFZero :: forall n. Fin n -> MatchFZero n
matchFZero (UnsafeMkFin 0) = unsafeAssumeSucc @n $ MFZYes
matchFZero _               = MFZNo

pattern FZero :: forall n. () => forall n'. (n ~ Succ n') => Fin n
pattern FZero <- (matchFZero -> MFZYes)
  where FZero = UnsafeMkFin 0

type MatchFSucc :: Nat -> Type
data MatchFSucc n where
  MFSYes :: Fin n -> MatchFSucc (Succ n)
  MFSNo  :: MatchFSucc n

matchFSucc :: forall n. Fin n -> MatchFSucc n
matchFSucc (UnsafeMkFin n) | n > 0     = unsafeAssumeSucc @n $ MFSYes (UnsafeMkFin (n-1))
                           | otherwise = MFSNo

pattern FSucc :: forall n. () => forall n'. (n ~ Succ n') => Fin n' -> Fin n
pattern FSucc f' <- (matchFSucc -> MFSYes f')
  where FSucc (UnsafeMkFin f) = UnsafeMkFin (f+1)

{-# COMPLETE FZero, FSucc #-}

intToFin :: SNat n -> Int -> Maybe (Fin n)
intToFin n f | snatToInt n > f, f >= 0 = Just (UnsafeMkFin f)
             | otherwise               = Nothing

unsafeIntToFin :: SNat n -> Int -> Fin n
unsafeIntToFin _ = UnsafeMkFin

freshFin :: Fin n -> Fin n
freshFin (UnsafeMkFin n) = UnsafeMkFin n
