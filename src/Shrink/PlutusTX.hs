module Shrink.PlutusTX
  (shrinkCompiled
  ,shrinkCompiledSp
  ) where

import Shrink (shrinkScriptSp,defaultShrinkParams)
import Shrink.Types

import Codec.Serialise              (serialise)
import Data.ByteString.Lazy         (toStrict)
import PlutusTx.Code                (CompiledCode, CompiledCodeIn (DeserializedCode, SerializedCode))
import UntypedPlutusCore            (programMapNames)
import Plutus.V1.Ledger.Scripts     (Script(Script),fromCompiledCode)
import PlutusCore.DeBruijn          (fakeNameDeBruijn)

shrinkCompiled :: CompiledCode a -> CompiledCode a
shrinkCompiled = shrinkCompiledSp defaultShrinkParams

shrinkCompiledSp :: ShrinkParams -> CompiledCode a -> CompiledCode a
shrinkCompiledSp sp comped = let
  asScript = fromCompiledCode comped
  script@(Script prog') = shrinkScriptSp sp asScript
  prog = programMapNames fakeNameDeBruijn prog'
  scriptBc = toStrict $ serialise script
    in case comped of
         SerializedCode   _ maybePirByteString -> SerializedCode   scriptBc maybePirByteString 
         DeserializedCode _ maybePir           -> DeserializedCode prog     maybePir
