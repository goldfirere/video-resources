{- *********************************************************************
*                                                                      *
                   Preliminaries
*                                                                      *
********************************************************************* -}

{-# LANGUAGE TypeOperators, TypeFamilies, StandaloneKindSignatures,
             DataKinds #-}

module Prelim
  ( Ty
  , Nat(..), type (+), type (-)
  ) where

import qualified Data.Kind as Kind
import Data.Type.Equality ( (:~:)(..) )
import Unsafe.Coerce ( unsafeCoerce )

type Ty = Kind.Type

{- *********************************************************************
*                                                                      *
                   Natural numbers
*                                                                      *
********************************************************************* -}

data Nat = Zero | Succ Nat    -- used only at compile time

type (+) :: Nat -> Nat -> Nat
type family n + m where
  Zero   + m = m
  Succ n + m = Succ (n + m)

type (-) :: Nat -> Nat -> Nat
type family n - m where
  n      - Zero = n
  Succ n - Succ m = n - m
