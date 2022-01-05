module Shrink.Tactics.Safe (
  safeTactList,
) where

import Shrink.Tactics.Util (completeRec, isData, mentions, subName', whnf)
import Shrink.Types (SafeTactic, WhnfRes (Err, Success, Unclear))

import Control.Monad (guard)

import PlutusCore.Default (DefaultFun (FstPair, MkPairData, SndPair))
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Delay, Error, Force, LamAbs, Var))

safeTactList :: [(String, SafeTactic)]
safeTactList = [("removeDeadCode", removeDeadCode), ("cleanPairs", cleanPairs), ("cleanForceDelay", cleanForceDelay), ("promoteErrors", promoteErrors)]

cleanPairs :: SafeTactic
cleanPairs = completeRec $ \case
  Apply
    _
    (Builtin _ FstPair)
    ( Apply
        _
        ( Apply
            _
            (Builtin _ MkPairData)
            fstTerm
          )
        sndTerm
      ) -> do
      guard $ isData fstTerm
      guard $ isData sndTerm
      return $ Delay () $ Delay () $ cleanPairs fstTerm
  Apply
    _
    (Builtin _ SndPair)
    ( Apply
        _
        ( Apply
            _
            (Builtin _ MkPairData)
            fstTerm
          )
        sndTerm
      ) -> do
      guard $ isData fstTerm
      guard $ isData sndTerm
      return $ Delay () $ Delay () $ cleanPairs sndTerm
  _ -> Nothing

removeDeadCode :: SafeTactic
removeDeadCode = completeRec $ \case
  (Apply _ (LamAbs _ name term) (Var _ name')) ->
    Just $ subName' name name' term
  -- subName' is used because name colision is intended in this case
  (Apply _ (LamAbs _ name term) val) ->
    case whnf val of
      Success () ->
        if mentions name term
          then Nothing
          else Just term
      Unclear -> Nothing
      Err -> Just $ Error ()
  _ -> Nothing

cleanForceDelay :: SafeTactic
cleanForceDelay = completeRec $ \case
  (Force _ (Delay _ t)) -> Just t
  (Force _ t) -> case cleanForceDelay t of
    Delay _ t' -> Just t'
    t' -> Just $ Force () t'
  _ -> Nothing

promoteErrors :: SafeTactic
promoteErrors = completeRec $ \case
  Error () -> Nothing
  t -> case whnf t of
    Err -> Just $ Error ()
    _ -> Nothing
