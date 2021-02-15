{-# LANGUAGE TypeFamilies, FlexibleInstances, DataKinds, StandaloneKindSignatures,
             PolyKinds, MultiParamTypeClasses #-}

module Records where

import Data.Kind

data A = MkA { a_field :: Maybe B }
data B = MKB { b_field :: Maybe C }
data C = MkC { c_field :: [D] }
data D = MkD { d_field :: E }
data E = MkE { e_field :: Maybe Int }

type List = []

type Sort :: Type -> (Type, Maybe (Type -> Type))
type family Sort field_type where
  Sort (Maybe a) = '(a, Just Maybe)
  Sort (List a)  = '(a, Just List)
  Sort other     = '(other, Nothing)

type family Fst a where
  Fst '(a, _) = a
type family Snd a where
  Snd '(_, b) = b

class Snd (Sort field) ~ wrapper => FieldType field wrapper where
  get :: Maybe r -> (r -> field) -> Maybe (Fst (Sort field))
  infixl 9 `get`

instance FieldType (Maybe b) (Just Maybe) where
  get = (>>=)

instance FieldType (List a) (Just List) where
  get (Just x) f | y:_ <- f x = Just y
                 | otherwise  = Nothing
  get Nothing  _              = Nothing

instance Sort something_else ~ '(something_else, Nothing) => FieldType something_else Nothing where
  get x f = fmap f x

goal :: A -> Maybe Int
goal a = Just a `get` a_field `get` b_field `get` c_field `get` d_field `get` e_field
