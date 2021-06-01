module Plugin (plugin) where

import Prelude hiding ( init )
import GHC.Plugins hiding ( TcPlugin )
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.TcType
import GHC.Core.Coercion
import GHC.Core.TyCo.Rep
import GHC.Core.Predicate
import GHC.Tc.Utils.Monad

import Control.Monad ( guard )

plugin = defaultPlugin { tcPlugin = \ _ -> Just tc_plugin }

tc_plugin :: TcPlugin
tc_plugin = TcPlugin { tcPluginInit = init
                     , tcPluginSolve = solve
                     , tcPluginStop = stop }

init :: TcPluginM ()
init = return ()

solve :: () -> TcPluginSolver
solve _ _givens _deriveds wanteds = do
  let mod_Prelim_name = mkModuleName "Prelim"
  find_result <- findImportedModule mod_Prelim_name Nothing
  mod_Prelim <- case find_result of Found _ mod -> return mod
                                    _           -> unsafeTcPluginTcM $
                                                   failWithTc (text "can't find Prelim module")
  let occ_plus = mkOccName tcName "+"
  plus_tc_name <- lookupOrig mod_Prelim occ_plus
  plus_tc <- tcLookupTyCon plus_tc_name

  let occ_zero = mkOccName dataName "Zero"
  zero_data_name <- lookupOrig mod_Prelim occ_zero
  zero_data <- tcLookupDataCon zero_data_name

  let
    solved :: [(EvTerm, Ct)]
    solved = [ (evidence, ct)
             | ct <- wanteds
             , Just (lhs_ty, rhs_ty) <- return (is_zero_on_right_pred (ctPred ct))
             , let prov = PluginProv "n+Zero=n"
                   evidence = evCoercion $ mkUnivCo prov Nominal lhs_ty rhs_ty  ]

    is_zero_on_right_pred :: TcPredType -> Maybe (TcType, TcType)
    is_zero_on_right_pred pred = do
      (Nominal, lhs_ty, rhs_ty) <- getEqPredTys_maybe pred
      guard (is_zero_on_right_tys lhs_ty rhs_ty)
      return (lhs_ty, rhs_ty)

    is_zero_on_right_tys :: TcType -> TcType -> Bool
    is_zero_on_right_tys lhs rhs = check lhs rhs || check rhs lhs

    check :: TcType -> TcType -> Bool
    check ty1 ty2
      | Just (tc1, [ty1_left, ty1_right]) <- splitTyConApp_maybe ty1
      , tc1 == plus_tc
      , ty1_left `eqType` ty2
      , Just (ty1_right_tc, []) <- splitTyConApp_maybe ty1_right
      , Just ty1_right_datacon <- isPromotedDataCon_maybe ty1_right_tc
      , ty1_right_datacon == zero_data
      = True

      | otherwise
      = False

  return (TcPluginOk solved [])

stop :: () -> TcPluginM ()
stop = return
