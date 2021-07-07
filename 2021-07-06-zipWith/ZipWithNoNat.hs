{-# LANGUAGE StandaloneKindSignatures, DataKinds, TypeOperators,
             TypeFamilies, UndecidableInstances, GADTs, ScopedTypeVariables,
             TypeApplications, FlexibleInstances, FlexibleContexts,
             ConstraintKinds, FunctionalDependencies #-}

module ZipWithNoNat where

import Prelude hiding ( zipWith )
import Data.Kind
import Data.Proxy

type ListsFrom :: Type -> Type
type family ListsFrom f where
  ListsFrom (arg -> res) = [arg] -> ListsFrom res
  ListsFrom other        = [other]

type ListsFromWitness :: Type -> Type
data ListsFromWitness f where
  ListsFromFun :: ListsFromClass res => ListsFromWitness (arg -> res)
  ListsFromNil :: ListsFrom other ~ [other] => ListsFromWitness other

type ListsFromClass :: Type -> Constraint
class ListsFromClass f where
  witness :: ListsFromWitness f

instance {-# OVERLAPPING #-} ListsFromClass res => ListsFromClass (arg -> res) where
  witness = ListsFromFun

instance {-# OVERLAPPABLE #-} ListsFrom other ~ [other] => ListsFromClass other where
  witness = ListsFromNil

zipWith :: forall f. ListsFromClass f => f -> ListsFrom f
zipWith fun = go (repeat fun)
  where
    go :: forall local_f. ListsFromClass local_f => [local_f] -> ListsFrom local_f
    go funs = case witness @local_f of
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

-- example3 = zipWith (+) [1,2,3] [4,5,6] -- type inference fails here
example3 = zipWith ((+) @Int) [1,2,3] [4,5,6]

{-
ListsFrom (a -> b -> c) = [a] -> [b] -> [c]
ListsFrom (a -> b -> c -> d) = [a] -> [b] -> [c] -> [d]
-}
