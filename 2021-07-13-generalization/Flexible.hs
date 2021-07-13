{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}

module Flexible where

class C a b | a -> b where
  op :: a -> b -> ()

foo x = op True x
