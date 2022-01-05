module Shrink (
  shrinkScript,
  shrinkScriptSp,
  withoutTactics,
  defaultShrinkParams,
  --testing exports
  traceShrinkDTerm,
  shrinkDTerm,
  size,
) where

import Shrink.Names (dTermToN, nTermToD)
import Shrink.Tactics.Safe (safeTactList)
import Shrink.Tactics.Tactics (tactList)
import Shrink.Types (DProgram, DTerm, MaybeTraceTerm, NTerm, SafeTactic, ShrinkParams (ShrinkParams, extraSteps, parallelTactics, parallelTerms, safeTactics, tactics), Tactic, Trace)

import Control.Arrow (first, second)
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Plutus.V1.Ledger.Scripts (Script (Script), scriptSize)
import UntypedPlutusCore (Version (Version))
import UntypedPlutusCore.Core.Type (Program (Program))

shrinkScript :: Script -> Script
shrinkScript = shrinkScriptSp defaultShrinkParams

shrinkScriptSp :: ShrinkParams -> Script -> Script
shrinkScriptSp sp (Script prog) = Script (shrinkProgramSp sp prog)

shrinkProgramSp :: ShrinkParams -> DProgram -> DProgram
shrinkProgramSp sp (Program _ version term) = Program () version (shrinkDTermSp sp term)

shrinkDTerm :: DTerm -> DTerm
shrinkDTerm = shrinkDTermSp defaultShrinkParams

traceShrinkDTerm :: DTerm -> (DTerm, Trace)
traceShrinkDTerm = traceShrinkDTermSp defaultShrinkParams

shrinkDTermSp :: ShrinkParams -> DTerm -> DTerm
shrinkDTermSp sp = nTermToD . shrinkNTermSp sp . dTermToN

shrinkNTermSp :: ShrinkParams -> NTerm -> NTerm
shrinkNTermSp sp = fst . shrinkMTraceTermSp sp . (,Nothing)

traceShrinkDTermSp :: ShrinkParams -> DTerm -> (DTerm, Trace)
traceShrinkDTermSp sp = first nTermToD . traceShrinkNTermSp sp . dTermToN

traceShrinkNTermSp :: ShrinkParams -> NTerm -> (NTerm, Trace)
traceShrinkNTermSp sp =
  second
    ( \case
        Just t -> t
        Nothing -> error "pretty sure this is unreachable but trace was dumped"
    )
    . shrinkMTraceTermSp sp
    . (,Just [])

shrinkMTraceTermSp :: ShrinkParams -> MaybeTraceTerm -> MaybeTraceTerm
shrinkMTraceTermSp sp = runShrink (extraSteps sp) sp . return

runShrink :: Integer -> ShrinkParams -> [MaybeTraceTerm] -> MaybeTraceTerm
runShrink es sp terms
  | sizeLast > sizeNow = runShrink (extraSteps sp) sp terms'
  | es > 0 = runShrink (es -1) sp terms'
  | otherwise = head terms
  where
    sizeNow = size $ fst $ head terms
    sizeLast = size $ fst $ head terms'
    terms' = stepShrink sp terms

stepShrink :: ShrinkParams -> [MaybeTraceTerm] -> [MaybeTraceTerm]
stepShrink sp terms =
  let cands = do
        tacts <- replicateM (fromIntegral $ parallelTactics sp) (tactics sp)
        foldl (>>=) terms (appTact <$> tacts)
      cands' = foldl (.) id (appSafe <$> safeTactics sp) <$> cands
      sizedCands = [(size $ fst c, c) | c <- cands']
      batches = groupBy ((==) `on` fst) . sortOn fst $ sizedCands
      uniques = concat $ fmap head . groupBy ((==) `on` (fst . snd)) . sortOn (show . fst . snd) <$> batches
   in take (fromIntegral $ parallelTerms sp) (snd <$> uniques)

-- TODO: possible performancce improvement right own compare instead of show

size :: NTerm -> Integer
size = sizeD . nTermToD
  where
    sizeD :: DTerm -> Integer
    sizeD = scriptSize . Script . Program () (Version () 0 0 0)

defaultShrinkParams :: ShrinkParams
defaultShrinkParams =
  ShrinkParams
    { safeTactics = safeTactList
    , tactics = tactList
    , parallelTactics = 1
    , parallelTerms = 20
    , extraSteps = 5
    }

withoutTactics :: [String] -> ShrinkParams
withoutTactics ts =
  defaultShrinkParams
    { safeTactics = filter (\(tn, _) -> tn `notElem` ts) (safeTactics defaultShrinkParams)
    , tactics = filter (\(tn, _) -> tn `notElem` ts) (tactics defaultShrinkParams)
    }

appSafe :: (String, SafeTactic) -> MaybeTraceTerm -> MaybeTraceTerm
appSafe (name, tact) (term, mtrace) = (tact term, ((name, 0) :) <$> mtrace)

appTact :: (String, Tactic) -> MaybeTraceTerm -> [MaybeTraceTerm]
appTact (name, tact) (term, mtrace) =
  [ (term', ((name, i) :) <$> mtrace)
  | (term', i) <- zip (tact term) [0 ..]
  ]
