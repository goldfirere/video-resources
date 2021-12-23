{-# LANGUAGE ExplicitForAll, StandaloneKindSignatures, PolyKinds,
             DataKinds, UndecidableInstances, TypeFamilyDependencies,
             MagicHash, UnboxedTuples, TypeOperators, TypeApplications #-}

module Tuple where

import Data.Kind
import GHC.Exts

x :: () -> (# #)
x _ = (# #)

data Solo a = Solo a

data Nat = Zero | Succ Nat

type TupleNKind :: Nat -> Type
type family TupleNKind n = r | r -> n where
  TupleNKind Zero     = Type
  TupleNKind (Succ n) = Type -> TupleNKind n

type TupleN :: forall (n :: Nat). TupleNKind n
type family TupleN where
  TupleN = ()
  TupleN = Solo
  TupleN = (,)
  TupleN = (,,)

type TupleNKind# :: [RuntimeRep] -> [RuntimeRep] -> Nat -> Type
type family TupleNKind# all_reps reps_to_go n_to_go = r | r -> all_reps reps_to_go n_to_go where
  TupleNKind# all_reps '[] Zero = TYPE (TupleRep all_reps)
  TupleNKind# all_reps (first_rep : reps_to_go) (Succ n) = TYPE first_rep -> TupleNKind# all_reps reps_to_go n

type Length :: [a] -> Nat
type family Length xs where
  Length '[] = Zero
  Length (_ : xs) = Succ (Length xs)

type TupleN# :: forall (reps :: [RuntimeRep]). TupleNKind# reps reps (Length reps)
type family TupleN# where
  TupleN# = (# #)
  TupleN# @'[rep1, rep2] = (# , #)
  TupleN# @'[rep1, rep2, rep3] = (# ,, #)

f :: TupleN# Int Int# Double# -> (# Int, Int#, Double# #)
f y = y
