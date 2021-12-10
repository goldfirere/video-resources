{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module TypeError where
import GHC.TypeLits hiding ( Nat, type (-) )
import Data.Kind

instance TypeError (Text "can't print out functions" :$$:
                    Text "specifically, can't print something of type " :<>: ShowType (a -> b)) => Show (a -> b) where
  show = error "you'll never see this"

data Nat = Zero | Succ Nat

type (-) :: Nat -> Nat -> Nat
type family a - b where
  a - Zero = a
  Succ a - Succ b = a - b
  Zero - b = TypeError (Text "negative natural: this would be -(" :<>: ShowType b :<>: Text ")")

type Fin :: Nat -> Type
data Fin n where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)

x :: Fin (Succ Zero - Succ (Succ Zero)) -> _
x = undefined

y = x