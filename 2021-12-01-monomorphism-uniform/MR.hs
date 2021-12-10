{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MR where

import System.Random

-- >>> :t uniform
-- uniform :: (RandomGen g, Uniform a) => g -> (a, g)

randomInt :: StdGen -> (Int, StdGen)
randomInt g0 = (n, g1)
  where
    (n :: Int, g1) = uniform g0

add5 = (+ 5)

randomIntPlus5 :: forall a. (Num a, Uniform a) => StdGen -> (a, StdGen)
randomIntPlus5 g0 = (add5 n, g1)
  where
    (n :: a, g1) = uniform g0
