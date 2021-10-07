{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main where
import NatSafe
import SNatUnsafe
import Data.Kind
import Data.Foldable
import VecUnsafe
import FinUnsafe

-- Prints out the first 100 'Fin's, after destroying any sharing.

main = withSNat 100 $ \n ->
       traverse_ print (fmap freshFin $ vecOfFins n)