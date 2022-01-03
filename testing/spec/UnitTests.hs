module UnitTests (makeUnitTests) where

import Paths_shrinker_testing (getDataFileName)

import Tactics (Similar ((~=)), run, testTacticOn, prettyPrintTerm)

import Shrink (defaultShrinkParams, shrinkDTerm, traceShrinkDTerm)
import Shrink.Names (dTermToN)
import Shrink.Types (DTerm, NTerm, Tactic, Trace, safeTactics, tactics)

import Control.Arrow (second)
import Control.Monad (filterM, when, (>=>))
import Data.Either (rights,isRight)
import Data.Functor ((<&>))
import Data.List (isInfixOf,sort)
import Data.Text (pack)
import Hedgehog (MonadTest, annotate, assert, failure, property, success)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import UntypedPlutusCore.Core.Type (Program (Program))

import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore.Assembler.AnnDeBruijn (annDeBruijn)
import PlutusCore.Assembler.Assemble (parseProgram)
import PlutusCore.Assembler.Desugar (desugar)

data TacticType = Safe | Unsafe deriving (Show)

makeUnitTests :: IO TestTree
makeUnitTests = do
  unitTestDir <- getDataFileName "./unitTests"
  unitTests <- (sort <$>)$ filterM doesFileExist . fmap (unitTestDir </>) =<< listDirectory unitTestDir
  srcs <- mapM (fmap pack . readFile) unitTests
  let uplcs'' = 
          [ (name,) <$> (parseProgram name >=> (fmap Script . desugar . annDeBruijn) $ src)
          | (name, src) <- zip unitTests srcs
          ]
  let uplcs' = rights uplcs''
      uplcs = [(name, uplc) | (name, Script (Program _ _ uplc)) <- uplcs']
  return $
    testGroup "Unit tests" $
      [ testProperty "unit-tests compile" . property $ do
        annotate $ show uplc
        assert $ isRight uplc
      | uplc <- uplcs'' ] ++
      (fullTest <$> uplcs) ++
        ( do
            (name, uplc) <- uplcs
            (tactName, tact) <-
              [Safe, Unsafe] >>= \case
                Unsafe -> tactics defaultShrinkParams
                Safe -> safeTactics defaultShrinkParams <&> second (return .)
            return $
              testProperty ("testing " ++ tactName ++ " on " ++ name) . property $ do
                when (('/' : tactName) `isInfixOf` name) $ testNonTrivial tact (dTermToN uplc)
                testTacticOn tactName tact (dTermToN uplc)
         )

testNonTrivial :: MonadTest m => Tactic -> NTerm -> m ()
testNonTrivial tact term = case tact term of
  [] -> do
    annotate "tactic returns []"
    failure
  [term'] -> do
    annotate "tactic is trivial"
    assert $ term /= term'
  (_ : _) -> success

fullTest :: (String, DTerm) -> TestTree
fullTest (name, uplc) = testProperty ("full test of shrink on " ++ name) . property $ do
  let uplc' = shrinkDTerm uplc
      start = dTermToN uplc
      end = dTermToN uplc'
      res1 = run start
      res2 = run end
      (term,tact,ind) = findCulprit uplc
  annotate $ "started with:" ++ prettyPrintTerm start
  annotate $ "which produced:" ++ show res1
  annotate $ "but shrank to:" ++ prettyPrintTerm end
  annotate $ "which produced:" ++ show res2
  annotate $ "tactics: " ++ tact ++ "broke"
  annotate $ "term: " ++ prettyPrintTerm term
  annotate $ "with option: " ++ show ind
  assert $ res1 ~= res2

findCulprit :: DTerm -> (NTerm,String,Int)
findCulprit uplc = let
  (_end,trace) = traceShrinkDTerm uplc
    in findCulprit' (dTermToN uplc) (reverse trace)

findCulprit' :: NTerm -> Trace -> (NTerm,String,Int)
findCulprit' _ [] = error "find culprit exhausted the trace without any errors, could be a non-transitivity issue"
findCulprit' uplc (nextStep@(tact,ind):trace) = let
  next = stepTrace nextStep uplc
    in if run uplc ~= run next 
          then findCulprit' next trace
          else (uplc,tact,ind)

stepTrace :: (String,Int) -> NTerm -> NTerm
stepTrace (tactName,ind) term = 
  case lookup tactName (safeTactics defaultShrinkParams) of
    Just safeTact -> safeTact term
    Nothing ->
      case lookup tactName (tactics defaultShrinkParams) of
        Just tact -> tact term !! ind
        Nothing -> error $ "unknown tactic: " ++ tactName




