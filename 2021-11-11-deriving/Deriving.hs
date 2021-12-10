{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Deriving where

import GHC.Generics hiding ( C )

import Data.Ord

import Data.Aeson

class C a

data T = MkT Int Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (C, FromJSON)
  -- instance C T

newtype Age = MkAge Int
  deriving stock (Eq)
  deriving Ord via (Down Int)
  deriving (Show, Num) via Int

-- >>> 5 < (6 :: Age)
-- False

-- >>> :i FromJSON
-- type FromJSON :: * -> Constraint
-- class FromJSON a where
--   parseJSON :: Value -> Parser a
--   default parseJSON :: (Generic a, GFromJSON Zero (Rep a)) =>
--                        Value -> Parser a
--   parseJSONList :: Value -> Parser [a]
--   	-- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON a => FromJSON [a]
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Word -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Value -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Ordering
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON a => FromJSON (Maybe a)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Key -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Integer -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Int -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Float -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b) => FromJSON (Either a b)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Double -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON DotNetTime
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Char -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON Bool -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i,
--           FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n,
--           FromJSON o) =>
--          FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i,
--           FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n) =>
--          FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i,
--           FromJSON j, FromJSON k, FromJSON l, FromJSON m) =>
--          FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i,
--           FromJSON j, FromJSON k, FromJSON l) =>
--          FromJSON (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i,
--           FromJSON j, FromJSON k) =>
--          FromJSON (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i,
--           FromJSON j) =>
--          FromJSON (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) =>
--          FromJSON (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g, FromJSON h) =>
--          FromJSON (a, b, c, d, e, f, g, h)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f, FromJSON g) =>
--          FromJSON (a, b, c, d, e, f, g)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e, FromJSON f) =>
--          FromJSON (a, b, c, d, e, f)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d,
--           FromJSON e) =>
--          FromJSON (a, b, c, d, e)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) =>
--          FromJSON (a, b, c, d)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a, b, c)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance (FromJSON a, FromJSON b) => FromJSON (a, b)
--   -- Defined in ‘Data.Aeson.Types.FromJSON’
-- instance FromJSON () -- Defined in ‘Data.Aeson.Types.FromJSON’

-- >>> MkT 3 True == MkT 4 True
-- No instance for (Eq T) arising from a use of ‘==’

-- >>> show (MkAge 5)
-- "5"
-- >>> MkAge 5 == MkAge 6
-- False
-- >>> MkAge 5 + MkAge 2
-- 7

f :: T -> String
f t = show t
