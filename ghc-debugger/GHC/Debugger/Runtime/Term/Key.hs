{-# LANGUAGE GADTs, ViewPatterns #-}
module GHC.Debugger.Runtime.Term.Key where

import Prelude hiding ((<>))

import GHC
import GHC.Utils.Outputable

-- | A 'TermKey' serves to fetch a Term in a Debugger session.
-- Note: A 'TermKey' is only valid in the stopped context it was created in.
data TermKey where
  -- | Obtain a term from an Id.
  FromId :: Id -> TermKey

  -- | Append a PathFragment to the current Term Key. Used to construct keys
  -- for indexed and labeled fields.
  FromPath :: TermKey -> PathFragment -> TermKey

-- | A term may be identified by an 'Id' (such as a local variable) plus a list
-- of 'PathFragment's to an arbitrarily nested field.
data PathFragment
  -- | A positional index is an index from 1 to inf
  = PositionalIndex Int
  -- | A labeled field indexes a datacon fields by name
  | LabeledField Name
  deriving (Eq, Ord)

instance Outputable TermKey where
  ppr (FromId i)          = ppr i
  ppr (FromPath _ last_p) = ppr last_p

instance Outputable PathFragment where
  ppr (PositionalIndex i) = text "_" <> ppr i
  ppr (LabeledField n)    = ppr n

-- | >>> unconsTermKey (FromPath (FromPath (FromId hi) (Pos 1)) (Pos 2))
-- (hi, [1, 2])
unconsTermKey :: TermKey -> (Id, [PathFragment])
unconsTermKey = go [] where
  go acc (FromId i) = (i, reverse acc)
  go acc (FromPath k p) = go (p:acc) k
