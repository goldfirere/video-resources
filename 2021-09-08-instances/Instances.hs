{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Instances where

class C a where
  meth :: a -> String

instance C Int where
  meth _ = "C Int"

instance C Bool where
  meth _ = "a thingy with Bool"

instance a ~ Int => C [a] where
  meth _ = "lists of numbers"

-- >>> f (3 :: Int)
-- "lists of numbers"
f = \y -> meth [y,y]
