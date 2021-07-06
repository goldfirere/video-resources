{-# LANGUAGE StandaloneKindSignatures, DataKinds, TypeOperators,
             TypeFamilies, UndecidableInstances, GADTs, ScopedTypeVariables,
             TypeApplications, FlexibleInstances, FlexibleContexts,
             ConstraintKinds, FunctionalDependencies #-}

module ZipWith where

import Prelude hiding ( zipWith )
import Data.Kind
import Data.Proxy

data Nat = Zero | Succ Nat

type CountArgs :: Type -> Nat
type family CountArgs f where
  CountArgs (arg -> res) = Succ (CountArgs res)
  CountArgs _other       = Zero

type ListsFrom :: Type -> Type
type family ListsFrom f where
  ListsFrom (arg -> res) = [arg] -> ListsFrom res
  ListsFrom other        = [other]

type ListsFromWitness :: Type -> Nat -> Type
data ListsFromWitness f n where
  ListsFromFun :: LFC res => ListsFromWitness (arg -> res) (Succ n)
  ListsFromNil :: ListsFrom other ~ [other] => ListsFromWitness other Zero

type LFC f = ListsFromClass f (CountArgs f)

type ListsFromClass :: Type -> Nat -> Constraint
class ListsFromClass f n where
  witness :: ListsFromWitness f n
  -- witness :: forall f. ListsFromClass f => ListsFromWitness f

instance (ListsFromClass res n, CountArgs res ~ n) => ListsFromClass (arg -> res) (Succ n) where
  witness = ListsFromFun

instance ListsFrom other ~ [other] => ListsFromClass other Zero where
  witness = ListsFromNil

zipWith :: forall f. LFC f => f -> ListsFrom f
zipWith fun = go (repeat fun)
  where
    go :: forall local_f. LFC local_f => [local_f] -> ListsFrom local_f
    go funs = case witness @local_f @(CountArgs local_f) of
      ListsFromNil -> funs
      ListsFromFun -> \ list1 -> go (apply funs list1)

    apply :: forall a b. [a -> b] -> [a] -> [b]
    apply (f:fs) (x:xs) = f x : apply fs xs
    apply _      _      = []

fun1 :: Int -> Bool -> Double
fun1 x True = fromIntegral x + 3.14
fun1 x False = fromIntegral x + 2.78

fun2 :: Char -> Bool -> String -> Int
fun2 c b s = length (show c ++ show b ++ show s)

-- example1 :: [Double]
example1 = zipWith fun1 [1,2,3] [True, False, True]

-- example2 :: [Int]
example2 = zipWith fun2 "abc" [True, False, True] ["hello", "goodbye", "hi"]

{-
ListsFrom (a -> b -> c) = [a] -> [b] -> [c]
ListsFrom (a -> b -> c -> d) = [a] -> [b] -> [c] -> [d]
-}
