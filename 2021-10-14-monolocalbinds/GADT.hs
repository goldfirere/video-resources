{-# LANGUAGE GADTs, PatternSynonyms, RankNTypes #-}
module GADT where

data G a where
  MkGInt   :: G Int
  MkGOther :: G a

pattern PatSyn :: forall a. () => (a ~ Int) => G a
pattern PatSyn = MkGInt

{-# COMPLETE PatSyn, MkGOther #-}