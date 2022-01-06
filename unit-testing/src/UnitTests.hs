module UnitTests (makeUnitTests) where

import Paths_shrinker_unit_testing (getDataFileName)

import Shrink.Testing.Tactics (Similar ((~=)), prettyPrintTerm, run, testTacticOn, (~/=))

import Shrink (defaultShrinkParams, shrinkDTerm, traceShrinkDTerm)
import Shrink.Names (dTermToN)
import Shrink.Types (DTerm, NTerm, Tactic, Trace, safeTactics, tactics)

import Control.Arrow (second)
import Control.Monad (filterM, when, (>=>))
import Data.Either (isRight, rights)
import Data.Functor ((<&>))
import Data.List (find, isPrefixOf, sort)
import Data.Text (pack)
import Hedgehog (MonadTest, annotate, assert, failure, property, success)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>),takeFileName)
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
  unitTests <- (sort <$>) $ filterM doesFileExist . fmap (unitTestDir </>) =<< listDirectory unitTestDir
  srcs <- mapM (fmap pack . readFile) unitTests
  let uplcs'' =
        [ (name,) <$> (parseProgram name >=> (fmap Script . desugar . annDeBruijn) $ src)
        | (name, src) <- zip unitTests srcs
        ]
  let uplcs' = rights uplcs''
      uplcs = [(name, uplc) | (name, Script (Program _ _ uplc)) <- uplcs']
  return $
    testGroup
      "Unit tests"
      [ testGroup
          "unit-tests compile"
          [ testProperty "unit-tests compile" . property $ do
            annotate $ show uplc
            assert $ isRight uplc
          | uplc <- uplcs''
          ]
      , testGroup
          "tactics are unit tested"
          [ testProperty (name ++ " has unit tests") . property $ do
            annotate name
            assert $ any (name `isPrefixOf`) (takeFileName . fst <$> uplcs)
          | name <- (fst <$> tactics defaultShrinkParams) ++ (fst <$> safeTactics defaultShrinkParams)
          ]
      , testGroup
          "full tests"
          (fullTest <$> uplcs)
      , testGroup "single-tactic unit tests" $ do
          (name, uplc) <- uplcs
          (tactName, tact) <-
            [Safe, Unsafe] >>= \case
              Unsafe -> tactics defaultShrinkParams
              Safe -> safeTactics defaultShrinkParams <&> second (return .)
          return $
            testProperty ("testing " ++ tactName ++ " on " ++ takeFileName name) . property $ do
              when (tactName `isPrefixOf` takeFileName name) $ testNonTrivial tact (dTermToN uplc)
              testTacticOn tactName tact (dTermToN uplc)
      ]

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
fullTest (name, uplc) = testProperty ("full test of shrink on " ++ takeFileName name) . property $ do
  let uplc' = shrinkDTerm uplc
      start = dTermToN uplc
      end = dTermToN uplc'
      res1 = run start
      res2 = run end
      (term, tact, ind) = findCulprit uplc
  annotate $ "started with:" ++ prettyPrintTerm start
  annotate $ "which produced:" ++ show res1
  annotate $ "but shrank to:" ++ prettyPrintTerm end
  annotate $ "which produced:" ++ show res2
  annotate $ "tactics: " ++ tact ++ "broke"
  annotate $ "term: " ++ prettyPrintTerm term
  annotate $ "with option: " ++ show ind
  assert $ res1 ~= res2

findCulprit :: DTerm -> (NTerm, String, Int)
findCulprit uplc =
  let (end, trace) = traceShrinkDTerm uplc
   in if run (dTermToN uplc) ~= run (dTermToN end)
        then error "trace doesn't break uplc"
        else case findCulprit' (dTermToN uplc) (reverse trace) of
          Just culprit -> culprit
          Nothing -> error $ "failed to find break in: " ++ show (reverse trace)

findCulprit' :: NTerm -> Trace -> Maybe (NTerm, String, Int)
findCulprit' term trace =
  let steps = genSteps term trace
      originalRes = run term
   in case find (\(step, _) -> run step ~/= originalRes) (zip (tail steps) trace) of
        Just (broken, (tact, ind)) -> Just (broken, tact, ind)
        Nothing -> Nothing

genSteps :: NTerm -> Trace -> [NTerm]
genSteps = scanl (flip stepTrace)

stepTrace :: (String, Int) -> NTerm -> NTerm
stepTrace (tactName, ind) term =
  case lookup tactName (safeTactics defaultShrinkParams) of
    Just safeTact -> safeTact term
    Nothing ->
      case lookup tactName (tactics defaultShrinkParams) of
        Just tact -> tact term !! ind
        Nothing -> error $ "unknown tactic: " ++ tactName
