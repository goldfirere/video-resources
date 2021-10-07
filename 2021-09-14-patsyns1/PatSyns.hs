{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns #-}
module PatSyns ( Card(.., CJack, CQueen, CKing, CAce), numCardsToPlay )where

import Numeric.Natural ( Natural )

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 
          | CHonor Honor
data Honor = HJack | HQueen | HKing | HAce

-- >>> show (CHonor HQueen)
-- "Q"
-- >>> show CAce
-- "A"
instance Show Card where
  show = \case
    C2 -> "2"; C3 -> "3"; C4 -> "4"; C5 -> "5"; C6 -> "6"
    C7 -> "7"; C8 -> "8"; C9 -> "9"; C10 -> "10"
    CJack -> "J"; CQueen -> "Q"; CKing -> "K"; CAce -> "A"

{-# COMPLETE C2, C3, C4, C5, C6, C7, C8, C9, C10, CJack, CQueen, CKing, CAce #-}
{-# LANGUAGE ViewPatterns #-}

pattern CJack = CHonor HJack
pattern CQueen = CHonor HQueen
pattern CKing = CHonor HKing
pattern CAce = CHonor HAce

numCardsToPlay :: Honor -> Natural
numCardsToPlay HJack = 1
numCardsToPlay HQueen = 2
numCardsToPlay HKing = 3
numCardsToPlay HAce = 4

pattern Even <- (even -> True)
  where Even = 42

-- >>> checkEven 4    -- even 4 == True ?
-- "yes it's even"
-- >>> checkEven 11   -- even 11 == True ?
-- "no it's not"
checkEven :: Int -> String
checkEven (even -> True) = "yes it's even"
checkEven _              = "no it's not"

-- >>> show Even
-- "42"
