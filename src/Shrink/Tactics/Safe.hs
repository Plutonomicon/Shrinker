module Shrink.Tactics.Safe (
  safeTactList,
) where

import Shrink.Tactics.Util
import Shrink.Types

import PlutusCore.Default (DefaultFun (..))
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Error, LamAbs, Var))

safeTactList :: [(String, SafeTactic)]
safeTactList = [("removeDeadCode", removeDeadCode), ("clean pairs", cleanPairs)]

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
        _
      ) -> Just $ cleanPairs fstTerm
  Apply
    _
    (Builtin _ SndPair)
    ( Apply
        _
        ( Apply
            _
            (Builtin _ MkPairData)
            _
          )
        sndTerm
      ) ->
      Just $ cleanPairs sndTerm
  _ -> Nothing

removeDeadCode :: SafeTactic
removeDeadCode = completeRec $ \case
  (Apply _ (LamAbs _ name term) (Var _ name')) ->
    Just $ subName' name name' term
  -- subName' is used because name colision is intended in this case
  (Apply _ (LamAbs _ name term) val) ->
    case whnf val of
      Success ->
        if mentions name term
          then Nothing
          else Just term
      Unclear -> Nothing
      Err -> Just $ Error ()
  _ -> Nothing
