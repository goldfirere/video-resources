{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module CheckWarnings where

import Language.Haskell.TH.Syntax

import GHC.Tc.Types
import GHC.Driver.Flags
import GHC.Driver.Session
import Unsafe.Coerce

myconst :: a -> b -> a
myconst = $( let runTcM :: TcM a -> Q a
                 runTcM action = Q (unsafeCoerce action)
             in
             do dflags <- runTcM getDynFlags
                if wopt Opt_WarnUnusedMatches dflags
                  then return (LamE [VarP (mkName "x"), WildP] (VarE (mkName "x")))
                  else return (LamE [VarP (mkName "x"), VarP (mkName "y")] (VarE (mkName "x"))) )
