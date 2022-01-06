module Shrink.Tactics.Safe (
  safeTactList,
) where

import Shrink.Tactics.Util (completeRec, completeSafeTactic, isData, mentions, sepMaybe, subName', whnf)
import Shrink.Types (SafeTactic, WhnfRes (Err, Success, Unclear))

import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))

import PlutusCore.Default (DefaultFun (FstPair, MkPairData, SndPair))
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Delay, Error, Force, LamAbs, Var))

safeTactList :: [(String, SafeTactic)]
safeTactList = [("removeDeadCode", removeDeadCode), ("cleanPairs", cleanPairs), ("cleanForceDelay", cleanForceDelay), ("promoteErrors", promoteErrors)]

cleanPairs :: SafeTactic
cleanPairs = completeSafeTactic $ \case
  Apply
    _
    (Builtin _ b)
    ( Apply
        _
        ( Apply
            _
            (Builtin _ MkPairData)
            fstTerm
          )
        sndTerm
      ) -> sepMaybe $ do
      guard =<< isData fstTerm
      guard =<< isData sndTerm
      case b of
        FstPair -> return $ Delay () $ Delay () $ cleanPairs fstTerm
        SndPair -> return $ Delay () $ Delay () $ cleanPairs sndTerm
        _ -> lift . lift $ Nothing
  _ -> return Nothing

removeDeadCode :: SafeTactic
removeDeadCode = completeSafeTactic $ \case
  (Apply _ (LamAbs _ name term) (Var _ name')) ->
    return $ Just $ subName' name name' term
  -- subName' is used because name colision is intended in this case
  (Apply _ (LamAbs _ name term) val) ->
    whnf val <&> \case
      Success () ->
        if mentions name term
          then Nothing
          else Just term
      Unclear -> Nothing
      Err -> Just $ Error ()
  _ -> return Nothing

cleanForceDelay :: SafeTactic
cleanForceDelay = completeRec $ \case
  (Force _ (Delay _ t)) -> Just t
  (Force _ t) -> case cleanForceDelay t of
    Delay _ t' -> Just t'
    t' -> Just $ Force () t'
  _ -> Nothing

promoteErrors :: SafeTactic
promoteErrors = completeSafeTactic $ \case
  Error () -> return Nothing
  t ->
    whnf t <&> \case
      Err -> Just $ Error ()
      _ -> Nothing
