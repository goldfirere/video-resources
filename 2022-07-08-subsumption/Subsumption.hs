{-# LANGUAGE RankNTypes #-}

module Main where

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

f :: Int -> Int -> forall a. Int
f x y = x + y

main :: IO ()
main = do
  print $ map (\x -> f (fib 25) x) [1, 2, 3]

-- why does type-checker eta-expand?
--   f :: Int -> Int -> forall a. Int
--   f (fib 25) :: Int -> forall a. Int
--   map :: (Int -> Int) -> [Int] -> [Int]
--   type-checker checks: is Int -> forall a. Int  more polymorphic than Int -> Int
--   type-checker says yes... but we must eta-expand!
--     if x :: Int, f (fib 25) x :: forall a. Int
--     if x :: Int, f (fib 25) x :: Int
--   \x -> f (fib 25) x :: Int -> Int

-- simple f (fib 25):                     17565920 bytes
-- eta-expanded:                          52526848 bytes
-- eta-contracted, but with extra forall: 52526848 bytes

-- GHC 8 and below: automatic eta expansion via deep subsumption
-- GHC 9.0 and 9.2: no automatic eta expansion; no deep subsumption
-- GHC 9.4 (and 9.2.4): yes deep subsumption... but only if you say -XDeepSubsumption (also -XDeepSubsumption is on with -XHaskell2010 or -XHaskell98)