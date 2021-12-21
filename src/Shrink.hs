module Shrink
  (shrinkScript
  ,shrinkScriptSp
  ,withoutTactics
  ,defaultShrinkParams
  ) where

import Shrink.Types
import Shrink.Names           (nTermToD,dTermToN)
import Shrink.Tactics.Tactics (tactList)
import Shrink.Tactics.Safe    (safeTactList)

import Control.Monad                (replicateM)
import Data.Function                (on)
import Data.List                    (group, groupBy, sortOn)
import Plutus.V1.Ledger.Scripts     (Script (Script), scriptSize)
import UntypedPlutusCore            (Version (Version))
import UntypedPlutusCore.Core.Type  (Program (Program))


shrinkScript :: Script -> Script
shrinkScript = shrinkScriptSp defaultShrinkParams

shrinkScriptSp :: ShrinkParams -> Script -> Script
shrinkScriptSp sp (Script prog) = Script (shrinkProgramSp sp prog)

shrinkProgramSp :: ShrinkParams -> DProgram -> DProgram
shrinkProgramSp sp (Program _ version term) = Program () version (shrinkDTermSp sp term)

shrinkDTermSp :: ShrinkParams -> DTerm -> DTerm
shrinkDTermSp sp = nTermToD . shrinkNTermSp sp . dTermToN

shrinkNTermSp :: ShrinkParams -> NTerm -> NTerm
shrinkNTermSp sp = runShrink (extraSteps sp) sp . return

runShrink :: Integer -> ShrinkParams -> [NTerm] -> NTerm
runShrink es sp !terms
  | sizeLast > sizeNow = runShrink (extraSteps sp) sp terms'
  | es > 0             = runShrink (es -1)         sp terms'
  | otherwise          = head terms
    where
      sizeNow  = size $ head terms
      sizeLast = size $ head terms'
      terms' = stepShrink sp terms

stepShrink :: ShrinkParams -> [NTerm] -> [NTerm]
stepShrink sp terms = let
  cands = do
    tacts <- replicateM (fromIntegral $ parallelTactics sp) (snd <$> tactics sp)
    foldl (>>=) terms tacts
  cands' = foldl (.) id (snd <$> safeTactics sp) <$> cands
  sizedCands = [(size c,c) | c <- cands' ]
  batches = groupBy ((==) `on` fst) . sortOn fst $ sizedCands
  uniques = concat $ fmap head . group . sortOn (show . snd) <$> batches
  in take (fromIntegral $ parallelTerms sp) (snd <$> uniques)

size :: NTerm -> Integer
size = sizeD . nTermToD
  where
    sizeD :: DTerm -> Integer
    sizeD = scriptSize . Script . Program () (Version () 0 0 0)

defaultShrinkParams :: ShrinkParams
defaultShrinkParams = ShrinkParams
  { safeTactics = safeTactList
  , tactics = tactList
  , parallelTactics = 1
  , parallelTerms = 20
  , extraSteps = 5
  }

withoutTactics :: [String] -> ShrinkParams
withoutTactics ts = defaultShrinkParams
  { safeTactics = filter (\(tn,_) -> tn `notElem` ts) (safeTactics defaultShrinkParams)
  , tactics     = filter (\(tn,_) -> tn `notElem` ts) (tactics     defaultShrinkParams)
  }

