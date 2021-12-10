{-# LANGUAGE LambdaCase, BlockArguments, ImpredicativeTypes #-}

module Trees where

import Test.QuickCheck
import Control.Applicative

data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
      gen :: Int -> Gen (Tree a)
      gen 0 = Leaf <$> arbitrary
      gen n = frequency [ (1, Leaf <$> arbitrary)
                        , (2, Node <$> gen (n `div` 2) <*> arbitrary <*> gen (n `div` 2)) ]

tree :: Tree Int
tree = Node (Node (Leaf (-22)) 29 (Node (Node (Leaf 19) (-4) (Node (Leaf 29) 4 (Leaf 1))) (-27) (Node (Leaf 21) 14 (Node (Leaf (-21)) 3 (Leaf 11))))) (-29) (Leaf 2)

type TreeIndex = forall a. Tree a -> Maybe a

rootIndex :: TreeIndex
rootIndex (Leaf x) = Just x
rootIndex (Node _ x _) = Just x

leftIndex :: TreeIndex -> TreeIndex
leftIndex _sub_index (Leaf _) = Nothing
leftIndex sub_index  (Node left _ _) = sub_index left

rightIndex :: TreeIndex -> TreeIndex
rightIndex _sub_index (Leaf _) = Nothing
rightIndex sub_index  (Node _ _ right) = sub_index right

lookupTree :: forall a. (a -> Bool) -> Tree a -> Maybe TreeIndex
lookupTree p = go
  where
    go :: Tree a -> Maybe TreeIndex
    go (Leaf x)
      | p x = Just rootIndex
      | otherwise = Nothing
    go (Node left x right) = asum (fmap leftIndex (go left) :
                                   (if p x then Just rootIndex else Nothing) :
                                   fmap rightIndex (go right) :
                                   [])

lookupsTree :: forall a. (a -> Bool) -> Tree a -> [TreeIndex]
lookupsTree p = go
  where
    go :: Tree a -> [TreeIndex]
    go (Leaf x)
      | p x = [rootIndex]
      | otherwise = []
    go (Node left x right) = concat (fmap leftIndex (go left) :
                                     (if p x then [rootIndex] else []) :
                                     fmap rightIndex (go right) :
                                     [])