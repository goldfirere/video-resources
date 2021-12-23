{-# LANGUAGE PolyKinds, FlexibleInstances #-}

module Poly where

import Data.Proxy
import Data.Kind

apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

-- f :: kappa1
-- a :: kappa2
-- kappa1 := kappa3 -> kappa4
-- kappa4 := Type
-- kappa3 := kappa2
-- f :: kappa2 -> Type
-- a :: kappa2
-- App :: (k -> Type) -> k -> Type
data App f a = MkApp (f a)

-- >>> :kind App
-- App :: (k -> *) -> k -> *

class C a where
  meth :: Proxy a -> Int
instance C (a :: (Type -> Type) -> Type -> Type) where
  meth _ = 5

data AtInt f = MkAtInt (f Int)

-- >>> :kind AtInt
-- AtInt :: (* -> *) -> *

-- >>> :kind Maybe
-- Maybe :: * -> *

unAtInt :: AtInt Maybe -> Maybe Int
unAtInt (MkAtInt x) = x

unBoth :: App AtInt Maybe -> Maybe Int
unBoth (MkApp (MkAtInt x)) = x
