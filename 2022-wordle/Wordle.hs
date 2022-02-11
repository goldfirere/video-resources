module Wordle where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T

-- KNOWN BUG: Does not work for words with repeated letters

import Words

-------------------------
-- utility

lettersInWord :: WordleWord -> S.Set Char
lettersInWord word = V.foldl' (flip S.insert) S.empty (getLetters word)

allLocations :: [Location]
allLocations = [0 .. 4]

-------------------------
-- responses to guesses

data LetterResponse = Gray | Yellow | Green

instance Show LetterResponse where
  show Gray = "X"
  show Yellow = "Y"
  show Green = "G"

newtype Response = MkResponse (Vec5 LetterResponse)

getResponses :: Response -> Vec5 LetterResponse
getResponses (MkResponse rs) = rs

instance Show Response where
  show (MkResponse responses) = concatMap show (V.toList responses)

respondToGuess :: WordleWord   -- guess
               -> WordleWord   -- answer
               -> Response     -- pattern of gray, yellow, and green boxes
respondToGuess guess answer = MkResponse (V.zipWith convert green_spots (getLetters guess))
  where
    answer_letters = lettersInWord answer

    green_spots :: Vec5 Bool
    green_spots = V.zipWith (==) (getLetters guess) (getLetters answer)

    convert :: Bool -> Char -> LetterResponse
    convert False the_letter | the_letter `S.member` answer_letters = Yellow
                             | otherwise                            = Gray
    convert True  _ = Green


-------------------------
-- Game state

-- for each letter in the alphabet, either:
--   1) we know nothing: not in the State map
--   2) it's not in the word
--   3) it's in the word, but not in some spots

type Location = Int   -- Int in the range 0 - 4

data LetterInformation
  = LetterNotInWord
  | LetterNotInLocations (S.Set Location)  -- set of places letter can *not* appear
  deriving Show

data State = MkState (M.Map Char LetterInformation)

instance Show State where
  show (MkState mapping) = show mapping

startingState :: State
startingState = MkState mempty

instance Semigroup State where
  (MkState map1) <> (MkState map2) = MkState (M.unionWith combine map1 map2)
    where
      combine :: LetterInformation -> LetterInformation -> LetterInformation
      combine LetterNotInWord LetterNotInWord = LetterNotInWord
      combine LetterNotInWord (LetterNotInLocations {}) = error "duplicate letter (1)"
      combine (LetterNotInLocations {}) LetterNotInWord = error "duplicate letter (2)"
      combine (LetterNotInLocations locs1) (LetterNotInLocations locs2) = LetterNotInLocations (locs1 <> locs2)

instance Monoid State where
  mempty = startingState

-------------------------------------
-- advancing state

advanceState :: State -> WordleWord -> Response -> State
advanceState state word response = state <> new_state
  where
     -- a new State reflecting only information in the Response
    new_state = MkState new_mapping

    guess_with_response :: Vec5 (Char, LetterResponse)
    guess_with_response = V.zip (getLetters word) (getResponses response)

    new_mapping = V.ifoldl' go mempty guess_with_response
      where
        go :: M.Map Char LetterInformation -> Int -> (Char, LetterResponse) -> M.Map Char LetterInformation
        go old_mapping _     (letter, Gray) = M.insert letter LetterNotInWord old_mapping
        go old_mapping index (letter, Yellow) = M.insert letter (LetterNotInLocations (S.singleton index)) old_mapping
        go old_mapping index (letter, Green) = M.insert letter (LetterNotInLocations (S.delete index (S.fromList allLocations))) old_mapping

--------------------------------------
-- filtering

-- set of all letters that are required, according to the state
requiredLetters :: State -> S.Set Char
requiredLetters (MkState mapping) = M.foldlWithKey' go S.empty mapping
  where
    go :: S.Set Char -> Char -> LetterInformation -> S.Set Char
    go already_required _current_letter LetterNotInWord = already_required
    go already_required current_letter (LetterNotInLocations {}) = current_letter `S.insert` already_required

validWord :: State -> WordleWord -> Bool
validWord state@(MkState mapping) word = no_letters_in_bad_locations word && all_required_letters word
  where
    required_letters = requiredLetters state

    -- check whether any letter is in a forbidden location
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

    -- all the required letters are indeed in the word
    all_required_letters :: WordleWord -> Bool
    all_required_letters word = required_letters `S.isSubsetOf` lettersInWord word


filterWords :: State -> V.Vector WordleWord -> V.Vector WordleWord
filterWords state words = V.filter (validWord state) words

------------------
-- examples

-- Suppose we get XXYXG for a guess of PEACH
peachState :: State
peachState = MkState (M.fromList [ ('p', LetterNotInWord)
                                 , ('e', LetterNotInWord)
                                 , ('a', LetterNotInLocations (S.fromList [2]))
                                 , ('c', LetterNotInWord)
                                 , ('h', LetterNotInLocations (S.fromList [0, 1, 2, 3])) ])