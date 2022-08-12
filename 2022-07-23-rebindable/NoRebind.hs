{-# LANGUAGE TypeApplications #-}

module NoRebind (
 NoRebind.fromInteger,
 NoRebind.fromRational,
 NoRebind.fromString,
 NoRebind.ifThenElse,
one, two, three,
 fromListN,
 ) where

one, two, three :: Int
one = 1
two = 2
three = 3

fromInteger :: Integer -> Bool
fromInteger = even

fromRational :: Rational -> String
fromRational = show . Prelude.fromRational @Double

fromString :: String -> Char
fromString = head

ifThenElse x y z = x + y + z

fromListN _ = and