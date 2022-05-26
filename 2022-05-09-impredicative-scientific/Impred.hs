{-# LANGUAGE GADTs, ImpredicativeTypes #-}

module Impred where

import Type.Reflection

import Data.Scientific

produce :: forall t. Typeable t => Maybe t
produce
  | Just HRefl <- typeRep @t `eqTypeRep` typeRep @Int = Just 42
  | Just HRefl <- typeRep @t `eqTypeRep` typeRep @Bool = Just False
  | App tycon _arg <- typeRep @t
  , Just HRefl <- tycon `eqTypeRep` typeRep @Maybe = Just Nothing
  | otherwise = Nothing

-- floatingOrInteger :: (RealFloat r, Integral i) => Scientific -> Either r i

convertScientific :: forall t. Typeable t => Scientific -> t
convertScientific sci = case fOrI sci of
  Left floating
    | Just HRefl <- typeRep @t `eqTypeRep` typeRep @Float -> floating
    | Just HRefl <- typeRep @t `eqTypeRep` typeRep @Double -> floating
  Right integer
    | Just HRefl <- typeRep @t `eqTypeRep` typeRep @Int -> integer
    | Just HRefl <- typeRep @t `eqTypeRep` typeRep @Integer -> integer

fOrI :: Scientific -> Either (forall r. RealFloat r => r) (forall i. Integral i => i)
fOrI sci
  | isFloating sci = Left (toRealFloat sci)
  | otherwise      = Right $ case floatingOrInteger sci of
      Left _ -> error "impossible"
      Right int -> int