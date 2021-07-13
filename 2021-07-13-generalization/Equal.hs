{-# LANGUAGE PartialTypeSignatures, TypeFamilies, ScopedTypeVariables #-}

module Equal where

outer :: forall q. Num q => q -> [q]
outer x = inner 5
  where
    -- inner :: (q ~ t) => t -> [q]
    inner y = [x, y]
      -- x :: q
      -- y :: alpha
      -- [W] alpha ~ q
