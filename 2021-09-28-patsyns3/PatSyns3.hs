{-# LANGUAGE PatternSynonyms, GADTs, RankNTypes, KindSignatures, MagicHash,
             UnboxedTuples #-}
{-# LANGUAGE PolyKinds #-}

module PatSyns3 where

import GHC.Exts
import Data.Kind

pattern Three <- 3

threeMatcher 3 yes no = yes
threeMatcher _ _   no = no

data KitchenSink a where
  MkKS :: forall a b. Show b => b -> a -> KitchenSink a

pattern KS :: forall a. (Eq a, Num a) => forall b. Show b => b -> KitchenSink a
pattern KS x = MkKS x 3

foo (KS x) = 4#
foo _      = 5#

foo' sink = ksMatcher sink (\ x -> 4#) (\ _ -> 5#)

ksMatcher :: forall (rep :: RuntimeRep) (res :: TYPE rep).
             forall a. (Eq a, Num a)
          => KitchenSink a
          -> (forall b. Show b => b -> res)
          -> ((# #) -> res)
          -> res
ksMatcher (MkKS x 3) yes no = yes x
ksMatcher _          yes no = no (# #)