{-# LANGUAGE GADTs #-}
module GHC.Debugger.Runtime.Term.Cache where

import GHC.Runtime.Eval
import GHC.Types.Var.Env

import GHC.Debugger.Runtime.Term.Key

import Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- * Term Cache
--------------------------------------------------------------------------------

-- | A term cache maps Names to Terms.
--
-- We use the term cache to avoid redundant computation forcing (unique) names
-- we've already forced before.
--
-- A kind of trie map from 'TermKey's. The Map entry for no-path-fragments is
-- the 'Term' of the original 'Id'.
type TermCache = TermKeyMap Term

-- | Lookup a 'TermKey' in a 'TermCache'.
-- Returns @Nothing@ for a cache miss and @Just@ otherwise.
lookupTermCache :: TermKey -> TermCache -> Maybe Term
lookupTermCache = lookupTermKeyMap

-- | Inserts a 'Term' for the given 'TermKey' in the 'TermCache'.
--
-- Overwrites existing values.
insertTermCache :: TermKey -> Term -> TermCache -> TermCache
insertTermCache = insertTermKeyMap

--------------------------------------------------------------------------------
-- * TermKeyMap
--------------------------------------------------------------------------------

-- | Mapping from 'TermKey' to @a@. Backs 'TermCache', but is more general.
type TermKeyMap a = IdEnv (Map [PathFragment] a)

-- | Lookup a 'TermKey' in a 'TermKeyMap'.
lookupTermKeyMap :: TermKey -> TermKeyMap a -> Maybe a
lookupTermKeyMap key tc = do
  let (i, path) = unconsTermKey key
  path_map <- lookupVarEnv tc i
  M.lookup path path_map

-- | Inserts a 'Term' for the given 'TermKey' in the 'TermKeyMap'.
--
-- Overwrites existing values.
insertTermKeyMap :: TermKey -> a -> TermKeyMap a -> TermKeyMap a
insertTermKeyMap key term tc =
  let
    (i, path) = unconsTermKey key
    new_map = case lookupVarEnv tc i of
      Nothing           -> M.singleton path term
      Just existing_map -> M.insert path term existing_map
  in extendVarEnv tc i new_map
