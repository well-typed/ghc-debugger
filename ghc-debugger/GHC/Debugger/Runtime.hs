{-# LANGUAGE GADTs, LambdaCase, NamedFieldPuns #-}
module GHC.Debugger.Runtime where

import Data.IORef
import Control.Monad.Reader
import qualified Data.List as L

import GHC
import GHC.Types.FieldLabel
import GHC.Tc.Utils.TcType
import GHC.Runtime.Eval

import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Runtime.Term.Cache
import GHC.Debugger.Monad

-- | Obtain the runtime 'Term' from a 'TermKey'.
--
-- The 'TermKey' will be looked up in the 'TermCache' to avoid recomputing the
-- 'Term' if possible. On a cache miss the Term will be reconstructed from
-- scratch and stored in the cache.
obtainTerm :: TermKey -> Debugger Term
obtainTerm key = do
  tc_ref <- asks termCache
  tc     <- liftIO $ readIORef tc_ref
  case lookupTermCache key tc of
    -- cache miss: reconstruct, then store.
    Nothing ->
      let
        -- For boring types we want to get the value as it is (by traversing it to
        -- the end), rather than stopping short and returning a suspension (e.g.
        -- for the string tail), because boring types are printed whole rather than
        -- being represented by an expandable structure.
        depth i = if isBoringTy (GHC.idType i) then maxBound else defaultDepth

        -- Recursively get terms until we hit the desired key.
        getTerm = \case
          FromId i -> GHC.obtainTermFromId (depth i) False{-don't force-} i
          FromPath k pf -> do
            term <- getTerm k >>= \case
              -- When the key points to a Suspension, the real thing should
              -- already be forced. It's just that the shallow depth meant we
              -- returned a Suspension nonetheless while recursing in `getTerm`.
              t@Suspension{} -> do
                t' <- seqTerm t
                -- update term cache with intermediate values?
                -- insertTermCache k t'
                return t'
              t -> return t
            return $ case term of
              Term{dc=Right dc, subTerms} -> case pf of
                PositionalIndex ix -> subTerms !! (ix-1)
                LabeledField fl    ->
                  case L.findIndex (== fl) (map flSelector $ dataConFieldLabels dc) of
                    Just ix -> subTerms !! (ix-1)
                    Nothing -> error "Couldn't find labeled field in dataConFieldLabels"
              NewtypeWrap{wrapped_term} ->
                wrapped_term -- regardless of PathFragment
              _ -> error "Unexpected term for the given TermKey"
       in do
        term <- getTerm key
        liftIO $ writeIORef tc_ref (insertTermCache key term tc)
        return term

    -- cache hit
    Just hit -> return hit

-- | A boring type is one for which we don't care about the structure and would
-- rather see "whole" when being inspected. Strings and literals are a good
-- example, because it's more useful to see the string value than it is to see
-- a linked list of characters where each has to be forced individually.
isBoringTy :: Type -> Bool
isBoringTy t = isDoubleTy t || isFloatTy t || isIntTy t || isWordTy t || isStringTy t
                || isIntegerTy t || isNaturalTy t || isCharTy t


