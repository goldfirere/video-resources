{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Deferred where

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

inorder (Leaf x) = x
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

{-
five = {- let badProof :: String ~ Int
           badProof = error "type error" in -}
       let list = [2 :: Int, "hello" {- `cast` badProof -}, 3 :: Int] in length list -- (list !! 0) + (list !! 2)

six :: Int
six = 6
-}