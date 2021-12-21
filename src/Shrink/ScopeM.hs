module Shrink.ScopeM
  (runScopeMT
  ,runScopeM
  ,runScopedTact
  ,liftScope
  ) where

import Shrink.Types

import qualified Data.Set as S

import Control.Monad.Reader         (ask, runReaderT)
import Control.Monad.State          (evalStateT, get, put, runState)
import Data.Functor.Identity        (runIdentity)

import UntypedPlutusCore.Core.Type  (Term (Apply, Delay, Force, LamAbs, Var))
import UntypedPlutusCore            (Name (nameUnique), Unique (unUnique))

runScopeMT :: Monad m => NTerm -> ScopeMT m a -> m a
runScopeMT nt smtma = let
  globalScope = usedScope nt
  free = fromIntegral$1+maximum (0:(unUnique . nameUnique <$> S.toList globalScope))
    in evalStateT (runReaderT smtma (globalScope,S.empty)) free

runScopeM :: NTerm -> ScopeM a -> a
runScopeM nt = runIdentity . runScopeMT nt

usedScope :: NTerm -> Scope
usedScope = \case
  Var _ n      -> S.singleton n
  LamAbs _ n t -> S.insert n (usedScope t)
  Force _ t    -> usedScope t
  Delay _ t    -> usedScope t
  Apply _ f x  -> S.union (usedScope f) (usedScope x)
  _            -> S.empty

liftScope :: Monad m => ScopeM a -> ScopeMT m a
liftScope sma = do
  s <- ask
  f <- get
  let (a,f') = runState (runReaderT sma s) f
  put f'
  return a

runScopedTact :: (NTerm -> ScopeM a) -> NTerm -> a
runScopedTact f nt = runScopeM nt (f nt)
