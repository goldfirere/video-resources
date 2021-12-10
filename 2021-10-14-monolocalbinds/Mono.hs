{-# LANGUAGE MonoLocalBinds, GADTs, ScopedTypeVariables #-}
module Mono where

{-
GADTs implies MonoLocalBinds
TypeFamilies implies MonoLocalBinds
-}

import GADT
{-
f :: a -> G a -> Int
f x MkGInt = x
f _ MkGOther = 5
-}

-- f x = let g y = (x, y) in (g 'x', g False)

h :: forall a. a -> G a -> (Bool, Bool)
h x g = let j :: (a ~ Int) => b -> (Bool, b)
            j y = (x == x, y)   -- need Eq a
        in case g of MkGInt -> j True
                     MkGOther -> (False, True)

{-
MonoLocalBinds means:
 when a local definition mentions a variable from an outer scope, the
 definition will *not* be generalized
-}