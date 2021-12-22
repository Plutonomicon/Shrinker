module Shrink.Types (
  DTerm,
  NTerm,
  DProgram,
  ScopeMT,
  ScopeM,
  Scope,
  SafeTactic,
  Tactic,
  PartialTactic,
  ScopedTactic,
  ShrinkParams (..),
  WhnfRes (..),
  MonadScope,
) where

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState, State, StateT)

import Data.Set (Set)
import PlutusCore.DeBruijn (DeBruijn)
import PlutusCore.Default (DefaultFun, DefaultUni)
import UntypedPlutusCore (Name)
import UntypedPlutusCore.Core.Type (Program, Term)

type DTerm = Term DeBruijn DefaultUni DefaultFun ()
type NTerm = Term Name DefaultUni DefaultFun ()
type DProgram = Program DeBruijn DefaultUni DefaultFun ()

type ScopeMT m = ReaderT (Scope, Scope) (StateT Integer m)
type ScopeM = ReaderT (Scope, Scope) (State Integer)
type Scope = Set Name

type SafeTactic = NTerm -> NTerm

-- safe tactics are shortening strategies which
-- can never be counter productive
type Tactic = NTerm -> [NTerm]

-- tactics are ways of shortening programs
-- because they can be counter productive
-- they return a list of terms gotten by
-- applying the tactic at different points
-- in the program. The head of the list is
-- reservered for the original term
type PartialTactic = NTerm -> ScopeM (Maybe [NTerm])
type ScopedTactic = NTerm -> ScopeM [NTerm]

--type ScopedSafe    = NTerm -> ScopeM NTerm

data ShrinkParams = ShrinkParams
  { safeTactics :: [(String, SafeTactic)]
  , tactics :: [(String, Tactic)]
  , parallelTactics :: Integer
  , parallelTerms :: Integer
  , extraSteps :: Integer
  }

-- Tactics are stored with strings so the tests can
-- automatically add the name to the name of the
-- property test

data WhnfRes = Err | Unclear | Success deriving (Eq, Ord)

class (MonadReader (Scope, Scope) m, MonadState Integer m) => MonadScope m

instance Monad m => MonadScope (ScopeMT m)
