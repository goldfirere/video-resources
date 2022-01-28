{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Loopy where

class C a where
  meth :: a -> a
class C a => D a
class C a => E a

{-
data C a = MkCDict { meth :: a -> a }
data D a = MkDDict (C a)
data E a = MkEDict (C a)

Property: No dictionary is ever bottom.
-}

instance (E b, b ~ Maybe a, C (Maybe a)) => D (Maybe a)
instance (D b, b ~ Maybe a) => E (Maybe a)

f :: D a => a -> a
f x = meth x

evaluateThis :: Maybe Int
evaluateThis = f (Just 5)

{-  E version:
dMaybeADict :: E (Maybe a) -> D (Maybe a)
dMaybeADict e_dict = case e_dict of MkEDict c_dict -> MkDDict c_dict
-}

{-
dMaybeADict :: C (Maybe a) -> D (Maybe a)
dMaybeADict c_dict = MkDDict c_dict
-}