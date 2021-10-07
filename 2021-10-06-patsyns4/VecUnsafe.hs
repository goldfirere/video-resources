{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VecUnsafe (
    Vec(..)
  , vecNth
  , vecOfFins
  , buildVec
   ) where

import Data.Vector
import FinUnsafe
import SNatUnsafe

newtype Vec n a = UnsafeMkVec { getVec :: Vector a }
  deriving newtype (Functor, Foldable)

vecNth :: Vec n a -> Fin n -> a
vecNth v index = getVec v ! finToInt index

vecOfFins :: SNat n -> Vec n (Fin n)
vecOfFins size = UnsafeMkVec $ generate (snatToInt size) (unsafeIntToFin size)

buildVec :: SNat n -> (Fin n -> a) -> Vec n a
buildVec size mk = UnsafeMkVec $ generate (snatToInt size) (mk . unsafeIntToFin size)