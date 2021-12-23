{-# OPTIONS_GHC -Woverlapping-patterns #-}     -- on by default
{-# OPTIONS_GHC -Wincomplete-patterns #-}      -- on with -W
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}  -- on with -Weverything in GHC 9.0- or -Wall in 9.2+
{-# LANGUAGE BangPatterns #-}
module Patterns where

data Void
data VoidLike = MkVL !Void

f :: VoidLike -> Int
f !_ = 6

-- >>> f (MkVL undefined)
-- 6

