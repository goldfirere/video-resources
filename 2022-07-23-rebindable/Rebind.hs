{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- RebindableSyntax implies NoImplicitPrelude
-- ... uses the fromInteger that's in scope for int literals
-- ... uses the fromRational that's in scope for decimal lits
-- ... with OverloadedStrings, uses the fromString that's in scope for string literals
-- ... uses ifThenElse for if ... then ... else ...
-- ... uses (>>=), (>>), fail that are in scope for do-notation
--     (but also see new extension -XQualifiedDo)
-- ... uses fromListN (and other functions) when OverloadedLists is on for list literals
-- ... uses in-scope names for Arrows code

module Rebind where

import Prelude ( (+), Bool(..) )
import NoRebind

x = 5

d = 3.14

hello = "hello"

bad = ""

n = if one then two else three

{-
action = do
  x <- f 53
  y x
-}

l = [2,4,6]