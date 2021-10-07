module NatSafe (
     Nat(..)
   ) where

-- | Unary natural numbers
data Nat = Zero | Succ Nat