{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module RecordUpdate where

type Booly a = Bool

type family Booly2 a where
  Booly2 a = Bool

data Rec a = MkRec { foo :: Int, bar :: a, baz :: Double }
           | MkRec2 { foo :: Int, other :: Char }

deriving instance Show a => Show (Rec a)

f x = x { baz = 2.71, other = 'y' }

-- f (MkRec { foo = f, bar = b1, baz = b2 }) = MkRec { foo = False, bar = 5, baz = b2 }

-- >>> f example
-- MkRec {foo = 5, bar = 5, baz = 3.14}
-- >>> f example2
-- /Users/rae/pCloud Drive/work/tweag/video-resources/2022-04-28-record-update/RecordUpdate.hs:16:7-28: Non-exhaustive patterns in record update

-- >>> :t (f example)
-- MkRec2 {foo = 5, bar = 5, other = 'x'}
-- (f example) :: Num a1 => Rec a1

example = MkRec { foo = 4, bar = 3, baz = 3.14 }
example2 = MkRec2 { foo = 7, other = 'x' }
