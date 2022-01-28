{-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE MonoLocalBinds #-}

module Defaulting where

import GHC.Exts

f x = ()
  where
    g y = let _ = x in y
    h _ = let _ = x in g 3#
