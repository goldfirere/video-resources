{-# LANGUAGE DataKinds, StandaloneKindSignatures, GADTs,
             StandaloneDeriving, DerivingStrategies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports #-}

module Vec where

import Data.Kind ( Type )
import Prelude ( Bool(..), Show, (&&), error )

{- This file contains most of the functions from Data.List, sorted
   in order of difficulty for implementing these on length-indexed
   vectors. We'll attempt a few each week, getting successively
   harder.

   For documentation, see
   https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html
-}

data Nat = Zero | Succ Nat

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving stock instance Show a => Show (Vec n a)

{-
-- these are pretty straightforward:
and
-}

and :: Vec n Bool -> Bool
and Nil = True
and (b :> bs) = b && and bs

{-
or
any
all
sum
product
maximum
minimum
foldl
foldl'
foldl1
foldl1'
foldr
foldr1
scanl
scanl'
scanl1
scanr
scanr1
isPrefixOf
isSuffixOf
isInfixOf
isSubsequenceOf
elem
notElem
lookup
find

-- these are a little harder:
head
-}

head :: Vec (Succ n) a -> a
head (x :> _) = x

{-
tail
init
-}

init :: Vec (Succ n) a -> Vec n a
init (_ :> Nil)         = Nil
init (x :> xs@(_ :> _)) = x :> init xs

{-
last
uncons
singleton
map
-}

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil       = Nil
map f (x :> xs) = f x :> map f xs

{-
zip
unzip
zipWith
reverse
mapAccumL
mapAccumR
insert
sort
null
length
-}

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- x :: SNat (Succ (Succ Zero))
-- x must be SSucc (SSucc SZero)

length :: forall n a. Vec n a -> SNat n
length Nil = SZero
length (_ :> xs) = SSucc (length xs)

{-
replicate
-}

replicate :: SNat n -> a -> Vec n a
-- replicate :: foreach (n :: Nat) -> a -> Vec n a
replicate SZero _ = Nil
replicate (SSucc n) x = x :> replicate n x

{-
(++)
take
drop
stripPrefix
splitAt
concat

-- these need fancy type families (quite hard):
intersperse
inits
tails
intercalate
subsequences
permutations
transpose

-- these need existentials:
concatMap
unfoldr
takeWhile
dropWhile
dropWhileEnd
filter
nub
delete
(\\)
union
intersect

-- these need Fin:
(!!)
elemIndex
elemIndices
findIndex
findIndices

-- these need custom GADTs to encode the right conditions:
span
break
partition
group
-}

{-
The functions listed here we will not attempt.

-- these are not illuminating:
zip3
zip4
zip5
zip6
zip7
zipWith3
zipWith4
zipWith5
zipWith6
zipWith7
unzip3
unzip4
unzip5
unzip6
unzip7

-- these are really about text, not about lists:
lines
words
unlines
unwords

-- these are straightforward generalizations:
nubBy
deleteBy
deleteFirstsBy
unionBy
intersectBy
groupBy
sortBy
insertBy
maximumBy
minimumBy
genericLength
genericTake
genericDrop
genericSplitAt
genericIndex
genericReplicate
sortOn

-- no infinite vectors:
no iterate
no iterate'
no repeat
no cycle
-}
