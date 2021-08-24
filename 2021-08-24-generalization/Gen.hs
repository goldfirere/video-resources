







{--------------------------------------------------------------------

        Generalization is hard, but somebody's got to do it

                     Richard A. Eisenberg
                       rae@richarde.dev
                         @RaeHaskell

                            Tweag

             Code at: https://richarde.dev/code.zip

                 Haskell Implementors' Workshop
                   Sunday, August 22, 2021
---------------------------------------------------------------------}








{-
Inferring a binding's type is done by GHC.Tc.Solver.simplifyInfer.
We focus on GHC.Tc.Solver.decideQuantification.
-----------------------------------------------------------------

To infer the type of a let-binding:
 1. Generate constraints
 2. Simplify constraints (using instance resolution and equational rewriting)
 3. Determine ungeneralizable ("mono") variables:
   a. Variables from an outer scope
   b. Variables mentioned in a constraint when the monomorphism restriction (MR) applies
   c. Variables related by equality constraints to the above
 4. Default levity, representation, and multiplicity variables (and kind variables if -XNoPolyKinds)
 5. Choose the quantified variables:
   a. Variables mentioned in the "body" of the inferred type
   b. Variables related to those in a constraint
 6. Choose the quantified constraints:
   a. Constraints that mention the quantified variables
   b. Various special cases for extra inclusion (-XImplicitParams) or exclusion (e.g. HasCallStack)
 7. Remove redundant superclasses.
-}













{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, NoMonoLocalBinds, DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Gen where

import qualified Control.Category as C
import GHC.Exts

x = 5

{-
wurble x = wurb 5
  where
    wurb y = x + y
-}

{-
type family TF a

inject :: a -> TF a
inject = undefined

f :: forall a. a -> ()
f x = ()
  where
    g :: forall b. (TF a ~ TF b) => b -> [TF a]
    g y = [inject x, inject y]
-}

-- iddy x = x

-- catId = C.id

{-
printed = show 5
measured = length []
-}

{-
class Frob a where
  frob :: a -> ()

class Zurp a where
  zurp :: a -> ()

class FrobZurp a b where
  frobZurp :: a -> b -> ()

frozu x = frobZurp x 5
-- fro_zu x = frob x `seq` zurp 5
-}

-- checkRefl x = x == x && not (x < x)

{-
Not discussed:
 - partial type signatures
 - mutual recursion (where some definitions might have type signatures)
 - implicit parameters
 - functional dependencies
 - "naughty" quantification (trying to quantify a kind variable when a non-quantifiable
     type depends on it)
 - implementation details
-}
