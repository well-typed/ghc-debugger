{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Stopped.Variables where

import GHC
import GHC.Types.FieldLabel
import GHC.Runtime.Eval
import GHC.Core.DataCon
import qualified GHC.Runtime.Debugger as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime
import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Utils

-- | 'TyThing' to 'VarInfo'. The 'Bool' argument indicates whether to force the
-- value of the thing (as in @True = :force@, @False = :print@)
tyThingToVarInfo :: TyThing -> Debugger VarInfo
tyThingToVarInfo = \case
  t@(AConLike c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  t@(ATyCon c)   -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  t@(ACoAxiom c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  AnId i -> do
    let key = FromId i
    term <- obtainTerm key
    termToVarInfo key term

-- | Construct the VarInfos of the fields ('VarFields') of the given 'TermKey'/'Term'
termVarFields :: TermKey -> Term -> Debugger VarFields
termVarFields top_key top_term =

  -- Make 'VarInfo's for the first layer of subTerms only.
  case top_term of
      -- Boring types don't get subfields
      _ | isBoringTy (GHCI.termType top_term) ->
        return NoFields

      Term{dc=Right dc, subTerms=_{- don't use directly! go through @obtainTerm@ -}} -> do
        case dataConFieldLabels dc of
          -- Not a record type,
          -- Use indexed fields
          [] -> do
            let keys = zipWith (\ix _ -> FromPath top_key (PositionalIndex ix)) [1..] (dataConOrigArgTys dc)
            IndexedFields <$> mapM (\k -> obtainTerm k >>= termToVarInfo k) keys
          -- Is a record data con,
          -- Use field labels
          dataConFields -> do
            let keys = map (FromPath top_key . LabeledField . flSelector) dataConFields
            LabeledFields <$> mapM (\k -> obtainTerm k >>= termToVarInfo k) keys
      NewtypeWrap{dc=Right dc, wrapped_term=_{- don't use directly! go through @obtainTerm@ -}} -> do
        case dataConFieldLabels dc of
          [] -> do
            let key = FromPath top_key (PositionalIndex 1)
            wvi <- obtainTerm key >>= termToVarInfo key
            return (IndexedFields [wvi])
          [fld] -> do
            let key = FromPath top_key (LabeledField (flSelector fld))
            wvi <- obtainTerm key >>= termToVarInfo key
            return (LabeledFields [wvi])
          _ -> error "unexpected number of Newtype fields: larger than 1"
      _ -> return NoFields


-- | Construct a 'VarInfo' from the given 'Name' of the variable and the 'Term' it binds
termToVarInfo :: TermKey -> Term -> Debugger VarInfo
termToVarInfo key term = do
  -- Make a VarInfo for a term
  let
    isThunk
     | Suspension{} <- term = True
     | otherwise = False
    ty = GHCI.termType term

    -- We scrape the subterms to display as the var's value. The structure is
    -- displayed in the editor itself by expanding the variable sub-fields
    termHead t
      -- But show strings and lits in full
      | isBoringTy ty = t
      | otherwise     = case t of
         Term{}                    -> t{subTerms = []}
         NewtypeWrap{wrapped_term} -> t{wrapped_term = termHead wrapped_term}
         _                         -> t
  varName <- display key
  varType <- display ty
  varValue <- display =<< GHCD.showTerm (termHead term)
  -- liftIO $ print (varName, varType, varValue, GHCI.isFullyEvaluatedTerm term)

  -- The VarReference allows user to expand variable structure and inspect its value.
  -- Here, we do not want to allow expanding a term that is fully evaluated.
  -- We only want to return @SpecificVariable@ (which allows expansion) for
  -- values with sub-fields or thunks.
  varRef <- do
    if GHCI.isFullyEvaluatedTerm term
       -- Even if it is already evaluated, we do want to display a
       -- structure as long if it is not a "boring type" (one that does not
       -- provide useful information from being expanded)
       -- (e.g. consider how awkward it is to expand Char# 10 and I# 20)
       && (isBoringTy ty || not (hasDirectSubTerms term))
     then do
        return NoVariables
     else do
        ir <- getVarReference key
        return (SpecificVariable ir)

  return VarInfo{..}
  where
    hasDirectSubTerms = \case
      Suspension{}   -> False
      Prim{}         -> False
      NewtypeWrap{}  -> True
      RefWrap{}      -> True
      Term{subTerms} -> not $ null subTerms

