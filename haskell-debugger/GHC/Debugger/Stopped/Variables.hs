{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Stopped.Variables where

import Data.IORef
import Control.Monad.Reader

import GHC
import GHC.Types.FieldLabel
import GHC.Runtime.Eval
import GHC.Core.DataCon
import GHC.Core.TyCo.Rep
import qualified GHC.Runtime.Debugger as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime
import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Runtime.Term.Cache
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
termToVarInfo key term0 = do
  -- Make a VarInfo for a term
  let
    ty = GHCI.termType term0

    -- Check for function types explicitly since they seem to always match Suspension
    -- but should not be shown as thunks in the UI.
    checkFn (FunTy _ _ _ _) = True
    checkFn (ForAllTy _ t) = checkFn t
    checkFn _ = False
    isFn = checkFn ty

    isThunk = if not isFn then
      case term0 of
        Suspension{} -> True
        _ -> False
      else False

  term <- if not isThunk && isBoringTy ty
            then forceTerm key term0 -- make sure that if it's an evaluated boring term then it is /fully/ evaluated.
            else pure term0

  let
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
  -- Pass type as value for functions since actual value is useless
  varValue <- if isFn
    then pure $ "<fn> :: " ++ varType
    else display =<< GHCD.showTerm (termHead term)
  -- liftIO $ print (varName, varType, varValue, GHCI.isFullyEvaluatedTerm term)

  -- The VarReference allows user to expand variable structure and inspect its value.
  -- Here, we do not want to allow expanding a term that is fully evaluated.
  -- We only want to return @SpecificVariable@ (which allows expansion) for
  -- values with sub-fields or thunks.
  varRef <- do
    if GHCI.isFullyEvaluatedTerm term ||
       -- Even if it is already evaluated, we do want to display a
       -- structure as long if it is not a "boring type" (one that does not
       -- provide useful information from being expanded)
       -- (e.g. consider how awkward it is to expand Char# 10 and I# 20)
       (not isThunk && (isBoringTy ty || not (hasDirectSubTerms term)))
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

-- | Forces a term to WHNF in the general case, or to NF in the case of 'isBoringTy'.
-- The term is updated at the given key.
forceTerm :: TermKey -> Term -> Debugger Term
forceTerm key term = do
  let ty = GHCI.termType term
  term' <- if isBoringTy ty
              -- deepseq boring types like String, because it is more helpful
              -- to print them whole than their structure.
              then deepseqTerm term
              else seqTerm term
  -- update cache with the forced term right away instead of invalidating it.
  asks termCache >>= \r -> liftIO $ modifyIORef' r (insertTermCache key term')
  return term'
