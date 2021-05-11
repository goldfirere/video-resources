{-# LANGUAGE DataKinds, StandaloneKindSignatures, GADTs,
             StandaloneDeriving, DerivingStrategies, ScopedTypeVariables,
             TypeOperators, TypeFamilies, ConstraintKinds,
             FlexibleInstances, MultiParamTypeClasses,
             IncoherentInstances, UndecidableSuperClasses,
             TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports #-}

module Vec where

import Data.Kind ( Type, Constraint )
import Prelude ( Bool(..), Show, (&&), error, undefined, Int, Num(..), otherwise, Eq(..), (||) )
import Data.Type.Equality ( (:~:)(..) )
import Unsafe.Coerce ( unsafeCoerce )

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
-}

snoc :: Vec n a -> a -> Vec (Succ n) a
snoc Nil x       = (x :> Nil)
snoc (y :> ys) x = y :> (ys `snoc` x)
{-
reverse :: Vec n a -> Vec n a
reverse Nil       = Nil
reverse (x :> xs) = reverse xs `snoc` x
-}
mPlusZero :: forall m. SNat m -> (m + Zero) :~: m
mPlusZero SZero = Refl
mPlusZero (SSucc n) = case mPlusZero n of Refl -> Refl
{-# NOINLINE mPlusZero #-}
{-# RULES "mPlusZero" forall m. mPlusZero m = unsafeCoerce Refl #-}

mPlusSucc :: forall n m. SNat m -> (m + Succ n) :~: Succ (m + n)
mPlusSucc SZero = Refl
mPlusSucc (SSucc m') = case mPlusSucc @n m' of Refl -> Refl
{-# NOINLINE mPlusSucc #-}
{-# RULES "mPlusSucc" forall m. mPlusSucc m = unsafeCoerce Refl #-}

reverse :: Vec n a -> Vec n a
reverse ys = go SZero Nil ys
  where
    go :: forall m p a. SNat m -> Vec m a -> Vec p a -> Vec (m + p) a
    go m acc Nil       = case mPlusZero m of Refl -> acc
    go m acc (x :> (xs :: Vec p_pred a)) = case mPlusSucc @p_pred m of Refl -> go (SSucc m) (x :> acc) xs
--    go acc ((:>) @p_pred x xs) = case mPlusSucc @m @p_pred of Refl -> go (x :> acc) xs

{-
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
-}

type (+) :: Nat -> Nat -> Nat
type family a + b where
  Zero + b = b
  Succ a + b = Succ (a + b)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ v = v                -- n ~ 0:  n + m  ==  Zero + m  == m: result = Vec m a
(x :> xs) ++ v = x :> (xs ++ v)   -- n ~ Succ n':  n + m   ==  Succ n' + m  ==   Succ (n' + m)
                                  -- xs ++ v :: Vec (n' + m) a

{-
take

take :: Int -> [a] -> [a]
take 0 _ = []
take _ []       = []
take n (x : xs) = x : take (n-1) xs
-}

type Min :: Nat -> Nat -> Nat
type family Min a b where
  Min Zero _ = Zero
  Min (Succ _) Zero = Zero
  Min (Succ n) (Succ m) = Succ (Min n m)

take :: SNat n -> Vec m a -> Vec (Min n m) a
take SZero _ = Nil
take (SSucc _) Nil       = Nil
take (SSucc n') (x :> xs) = x :> take n' xs
{-
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
-}

type EVec :: (Nat -> Constraint) -> Type -> Type
data EVec c a where
  MkEVec :: c n => Vec n a -> EVec c a

type KnowNothing :: Nat -> Constraint
class KnowNothing n
instance KnowNothing n

type (>=) :: Nat -> Nat -> Constraint
class m >= n
instance m >= Zero
instance (m >= n) => (Succ m >= Succ n)
instance {-# OVERLAPPABLE #-} (m >= n) => (Succ m >= n)

filter :: forall n a. (a -> Bool) -> Vec n a -> EVec ((>=) n) a
filter _ Nil = MkEVec Nil
filter p (x :> xs) | p x       = case filter p xs of MkEVec tail -> MkEVec (x :> tail)
                   | otherwise = case filter p xs of MkEVec v -> MkEVec v

{-
nub
delete
(\\)
union
-}

elem :: Eq a => a -> Vec n a -> Bool
elem _ Nil = False
elem x (y :> ys) = (x == y) || elem x ys

type (<=) :: Nat -> Nat -> Constraint
class m <= n
instance n <= n
instance Zero <= n
instance (n <= m) => (n <= Succ m)

type (<&&>) :: (Nat -> Constraint) -> (Nat -> Constraint) -> Nat -> Constraint
class (cond1 n, cond2 n) => (cond1 <&&> cond2) n
instance (cond1 n, cond2 n) => (cond1 <&&> cond2) n

union :: Eq a => Vec n a -> Vec m a -> EVec ((<=) m) a
union xs Nil = MkEVec xs
union Nil ys = MkEVec ys
union (x :> xs) ys | x `elem` ys = case union xs ys of MkEVec u -> MkEVec u
                   | otherwise   = case union xs ys of MkEVec u -> MkEVec (x :> u)

{-
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
