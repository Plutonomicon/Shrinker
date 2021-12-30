{-# LANGUAGE TemplateHaskell #-}

module Shrink.PlutusTXTH (shrinkCompiledTH) where

import Language.Haskell.TH (Q, TExp)
import PlutusTx.Code (CompiledCode)
import Shrink.PlutusTX (shrinkCompiled)

shrinkCompiledTH :: Q (TExp (CompiledCode a)) -> Q (TExp (CompiledCode a))
shrinkCompiledTH q = [||shrinkCompiled $$(q)||]
