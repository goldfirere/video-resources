{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Generic where

-- 3 mechanisms: Typeable, Data, Generic

import Type.Reflection   -- Data.Typeable also works
import Data.Data ( Data )
import Data.Generics ( everywhere, mkT )
import GHC.Generics

-- Typeable allows runtime type identification

doSomethingSpecialOnInts :: Typeable a => a -> a
doSomethingSpecialOnInts x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @Int = x + 1
  | otherwise                                       = x

-- >>> doSomethingSpecialOnInts (5 :: Int)
-- 6

-- >>> :t eqTypeRep
-- eqTypeRep :: TypeRep a -> TypeRep b -> Maybe (a :~~: b)
-- >>> :t typeOf
-- typeOf :: Typeable a => a -> TypeRep a
-- >>> :t typeRep
-- typeRep :: Typeable a => TypeRep a

-- >>> :i :~~:
-- type (:~~:) :: forall k1 k2. k1 -> k2 -> *
-- data (:~~:) a b where
--   HRefl :: a :~~: a
--   	-- Defined in ‘Data.Type.Equality’

data Wurble = MkW Int Bool Int Double
  deriving (Show, Data)

add1 :: Data a => a -> a
add1 = everywhere (mkT ((+1) :: Int -> Int))

-- >>> add1 (MkW 2 True 3 8)
-- MkW 3 True 4 8.0

-- >>> add1 [MkW 2 True 3 8, MkW 10 False 15 42]
-- [MkW 3 True 4 8.0,MkW 11 False 16 42.0]

-- >>> :t everywhere
-- everywhere :: (forall a. Data a => a -> a) -> forall a. Data a => a -> a
-- >>> :t mkT
-- mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a

data Record = MkR { field :: Int, other_field :: Bool }
  deriving Generic

-- extractField @"field" r ...

-- libraries that work with generics:
--   generic-lens
--   generics-sop

-- >>> :kind! (Rep Record)
-- (Rep Record) :: * -> *
-- = D1
--     ('MetaData "Record" "Generic" "main" 'False)
--     (C1
--        ('MetaCons "MkR" 'PrefixI 'True)
--        (S1
--           ('MetaSel
--              ('Just "field")
--              'NoSourceUnpackedness
--              'NoSourceStrictness
--              'DecidedLazy)
--           (Rec0 Int)
--         :*: S1
--               ('MetaSel
--                  ('Just "other_field")
--                  'NoSourceUnpackedness
--                  'NoSourceStrictness
--                  'DecidedLazy)
--               (Rec0 Bool)))
