{-# LANGUAGE PolyKinds #-}

module Mono where

import Poly
import Data.Proxy

type MyApp = App

-- >>> :kind MyApp
-- MyApp :: (* -> *) -> * -> *

x :: Int
x = meth (Proxy :: Proxy App)

-- >>> x
-- 5
