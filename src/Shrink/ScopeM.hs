module Shrink.ScopeM (
  runScopeMT,
  runScopeM,
  runScopedTact,
  liftScope,
  newName,
) where

import Shrink.Types (MonadScope, NTerm, ScopeM, ScopeMT)

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set (Set)

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (evalStateT, get, modify, put, runState)
import Data.Functor.Identity (runIdentity)
import Data.Text (pack)

import UntypedPlutusCore (Name (Name, nameUnique), Unique (Unique, unUnique))
import UntypedPlutusCore.Core.Type (Term (Apply, Delay, Force, LamAbs, Var))

runScopeMT :: Monad m => NTerm -> ScopeMT m a -> m a
runScopeMT nt smtma =
  let nameSpace = usedScope nt
      free = fromIntegral $ 1 + maximum (0 : (unUnique . nameUnique <$> S.toList nameSpace))
   in evalStateT (runReaderT smtma M.empty) free

runScopeM :: NTerm -> ScopeM a -> a
runScopeM nt = runIdentity . runScopeMT nt

usedScope :: NTerm -> Set Name
usedScope = \case
  Var _ n -> S.singleton n
  LamAbs _ n t -> S.insert n (usedScope t)
  Force _ t -> usedScope t
  Delay _ t -> usedScope t
  Apply _ f x -> S.union (usedScope f) (usedScope x)
  _ -> S.empty

liftScope :: Monad m => ScopeM a -> ScopeMT m a
liftScope sma = do
  s <- ask
  f <- get
  let (a, f') = runState (runReaderT sma s) f
  put f'
  return a

runScopedTact :: (NTerm -> ScopeM a) -> NTerm -> a
runScopedTact f nt = runScopeM nt (f nt)

newName :: MonadScope m => m Name
newName = do
  n <- get
  modify (+ 1)
  return $ Name (pack $ show n) (Unique $ fromIntegral n)
