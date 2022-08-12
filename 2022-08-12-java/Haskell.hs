{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}

module Haskell where

-- based on https://logic.puzzlebaron.com/play.php?u2=a2abc96da0870644f11583f67e091fd6

{-
1. Shamir is 7 years old.
2. Shamir came from Ambalat.
3. Quirrel is younger than the ape that was found in Tarakan.
4. Of Ofallo and the ape that was found in Tarakan, one is cared for by Gracie and the other is 13 years old.
5. The animal that was found in Ambalat is either the 10-year-old or the animal Francine works with.
6. Ofallo isn't 10 years old.
7. The ape that was found in Kendisi is older than the ape Dolly works with.
-}

import Data.List
import Data.Maybe
import Control.Monad

data Orangutan = Merah | Ofallo | Quirrel | Shamir
  deriving (Enum, Bounded, Show, Eq)

data Handler = Dolly | Eva | Francine | Gracie
  deriving (Enum, Bounded, Show, Eq)

data Location = Ambalat | Basahan | Kendisi | Tarakan
  deriving (Enum, Bounded, Show, Eq)

orangutans = [minBound .. maxBound]
handlers = [minBound .. maxBound]
locations = [minBound .. maxBound]
ages = [4, 7, 10, 13]

data Assignment =
  MkAssignment { orangutan :: Orangutan, handler :: Handler, location :: Location, age :: Int }
  deriving (Eq)

instance Show Assignment where
  show (MkAssignment { .. }) = "(" ++ show orangutan ++ "," ++ show handler ++ "," ++ show location ++ "," ++ show age ++ ")"

type Solution = [Assignment]

answers :: [Solution]
answers = do
  solution@[merah, ofallo, quirrel, shamir]
    <- [ zipWith4 MkAssignment orangutans hs ls as
       | hs <- permutations handlers
       , ls <- permutations locations
       , as <- permutations ages ]

    -- Clue 1
  guard (shamir.age == 7)

    -- Clue 2
  guard (shamir.location == Ambalat)

    -- Clue 3
  Just tarakan <- [find (\a -> a.location == Tarakan) solution]
  guard (quirrel.age < tarakan.age)

    -- Clue 4
  let clue4 a1 a2 = a1.handler == Gracie && a2.age == 13
  guard (clue4 ofallo tarakan || clue4 tarakan ofallo)
  guard (ofallo /= tarakan)

    -- Clue 5
  Just ambalat <- [find (\a -> a.location == Ambalat) solution]
  guard (ambalat.age == 10 || ambalat.handler == Francine)

    -- Clue 6
  guard (ofallo.age /= 10)

    -- Clue 7
  Just kendisi <- [find (\a -> a.location == Kendisi) solution]
  Just dolly <- [find (\a -> a.handler == Dolly) solution]
  guard (kendisi.age > dolly.age)

  return solution

main :: IO ()
main = case answers of
  [answer] -> print answer
  _ -> putStrLn $ "Non-unique answers: " ++ show answers