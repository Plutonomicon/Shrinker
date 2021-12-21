module Shrink.Names 
  (nTermToD
  ,dTermToN
  )where

import Shrink.Types (DTerm,NTerm)
import PlutusCore   (FreeVariableError, runQuoteT)
import PlutusCore.DeBruijn          (fakeNameDeBruijn)
import UntypedPlutusCore            (deBruijnTerm, termMapNames, unDeBruijnTerm, unNameDeBruijn)

nTermToD :: NTerm -> DTerm
nTermToD = termMapNames unNameDeBruijn . (\case
  Right t -> t
  Left s  -> error $ "nTermToD failed with" ++ show (s :: FreeVariableError)
  ) . runQuoteT . deBruijnTerm

dTermToN :: DTerm -> NTerm
dTermToN = (\case
  Right t -> t
  Left s  -> error $ "dTermToN failed with" ++ show (s :: FreeVariableError)
  ) . runQuoteT . unDeBruijnTerm . termMapNames fakeNameDeBruijn
