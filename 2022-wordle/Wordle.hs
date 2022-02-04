module Wordle where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Words

-- for each letter in the alphabet, either:
--   1) we know nothing: not in the State map
--   2) it's not in the word
--   3) it's in the word, but not in some spots

type Location = Int   -- Int in the range 0 - 4

data LetterInformation
  = LetterNotInWord
  | LetterNotInLocations (S.Set Location)  -- set of places letter can *not* appear

data State = MkState (M.Map Char LetterInformation)

filterWords :: State -> V.Vector WordleWord -> V.Vector WordleWord
filterWords (MkState mapping) words = V.filter (\w -> no_letters_in_bad_locations w && all_required_letters w) words
  where
    no_letters_in_bad_locations :: WordleWord -> Bool
    no_letters_in_bad_locations word = V.ifoldl' go True (getLetters word)
      where
        go :: Bool -> Int -> Char -> Bool
        go False _index _letter = False
        go True index letter = case M.lookup letter mapping of
           -- letter hasn't been seen before:
          Nothing -> True
           -- see a letter that we know is not in the word. Reject!
          Just LetterNotInWord -> False
          Just (LetterNotInLocations excluded_locations) -> not (index `S.member` excluded_locations)

    required_letters = M.foldlWithKey' go S.empty mapping
      where
        go :: S.Set Char -> Char -> LetterInformation -> S.Set Char
        go already_required _current_letter LetterNotInWord = already_required
        go already_required current_letter (LetterNotInLocations {}) = current_letter `S.insert` already_required

    all_required_letters :: WordleWord -> Bool
    all_required_letters word = required_letters `S.isSubsetOf` letters_in_word
      where
        letters_in_word = V.foldl' (flip S.insert) S.empty (getLetters word)

-- Suppose we get XXYXG for a guess of PEACH
peachState :: State
peachState = MkState (M.fromList [ ('p', LetterNotInWord)
                                 , ('e', LetterNotInWord)
                                 , ('a', LetterNotInLocations (S.fromList [2]))
                                 , ('c', LetterNotInWord)
                                 , ('h', LetterNotInLocations (S.fromList [0, 1, 2, 3])) ])