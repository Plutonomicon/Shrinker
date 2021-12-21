{-# LANGUAGE TemplateHaskell #-}

module Shrink.PlutusTXTH
  (shrinkCompiledTH) where


import Language.Haskell.TH
import Shrink.PlutusTX     (shrinkCompiled)
import PlutusTx.Code       (CompiledCode)

shrinkCompiledTH :: Q (TExp (CompiledCode a)) -> Q (TExp (CompiledCode a))
shrinkCompiledTH q = [|| shrinkCompiled $$( q ) ||]
