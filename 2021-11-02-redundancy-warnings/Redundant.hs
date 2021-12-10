{-# LANGUAGE FlexibleInstances, UndecidableInstances, MonoLocalBinds #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module Redundant where

-- class Eq a => Ord a

f :: (Eq a, Ord a) => a -> Bool
f x = x == x

g :: (Eq a, Ord a) => a -> Bool
g x = x == x && x > x

h :: Ord a => a -> Bool
h x = x == x

class (Show a, Monoid a) => ShowMonoid a
instance (Show a, Monoid a) => ShowMonoid a

j :: ShowMonoid a => a -> String
j x = show (x <> x)
