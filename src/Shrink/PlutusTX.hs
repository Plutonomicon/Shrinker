module Shrink.PlutusTX (
  shrinkCompiled,
  shrinkCompiledSp,
) where

import Shrink (defaultShrinkParams, shrinkScriptSp)
import Shrink.Types (ShrinkParams)

import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Plutus.V1.Ledger.Scripts (Script (Script), fromCompiledCode)
import PlutusCore.DeBruijn (fakeNameDeBruijn)
import PlutusTx.Code (CompiledCode, CompiledCodeIn (DeserializedCode, SerializedCode))
import UntypedPlutusCore (programMapNames)

shrinkCompiled :: CompiledCode a -> CompiledCode a
shrinkCompiled = shrinkCompiledSp defaultShrinkParams

shrinkCompiledSp :: ShrinkParams -> CompiledCode a -> CompiledCode a
shrinkCompiledSp sp comped =
  let asScript = fromCompiledCode comped
      script@(Script prog') = shrinkScriptSp sp asScript
      prog = programMapNames fakeNameDeBruijn prog'
      scriptBc = toStrict $ serialise script
   in case comped of
        SerializedCode _ maybePirByteString coverageIndex -> SerializedCode scriptBc maybePirByteString coverageIndex
        DeserializedCode _ maybePir coverageIndex -> DeserializedCode prog maybePir coverageIndex
