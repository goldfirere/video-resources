{-# LANGUAGE FlexibleInstances #-}

module MR where

class C a where
  method :: a -> ()
instance C (Int, b) where
  method _ = ()

-- x :: beta
-- [W] Num beta
x = 5  -- MR here!
-- LHS does not look like a function; no type signature --> MR

x2 :: Int
x2 = x + 1

g = \y -> method (x, y)
-- g :: alpha -> ()
-- [W] C (beta, alpha)
-- might imagine: g :: forall a. C (beta, a) => a -> ()
-- but the MR says NO!
-- MR: g :: alpha -> ()
-- but wait: we know beta := Int
-- [W] C (Int, alpha)   <-- instance solves this constraint!
-- we are free to generalise g :: forall b. b -> ()

x3 :: ((), ())
x3 = (g True, g 'x')
