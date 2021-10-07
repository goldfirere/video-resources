{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module PatSyns2 where

pattern Truish :: Bool
pattern Truish = True

pattern Justish :: a -> Maybe a
pattern Justish a = Just a

f :: (Num a, Eq a) => a -> Bool
f 3 = True

pattern Three :: forall a. (Eq a, Num a) => () => a
pattern Three = 3

g :: Int -> Bool
g Three = False

data T where
  MkT :: (Eq a, Num a) => a -> T

h :: T -> Bool
h (MkTish x) = x == 3

{-# COMPLETE MkTish #-}

pattern MkTish :: forall. ()               -- "required"
               => forall a. (Eq a, Num a)  -- "provided"
               => a -> T
{-
pattern MkTish :: { universals = []
                  , required = ()
                  , existentials = [a]
                  , provided = (Eq a, Num a)
                  , arguments = [a]
                  , result = T }
-}
pattern MkTish x <- MkT x

pattern Twice :: b -> b -> Maybe b
pattern Twice x y <- (( \ (Just z) -> (z, z)) -> (x, y))