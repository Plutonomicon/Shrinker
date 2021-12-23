module UnitTests (makeUnitTests) where

import Paths_shrinker (getDataFileName)

import Tactics (Similar ((~=)), run, testTacticOn)

import Shrink (defaultShrinkParams, shrinkDTerm)
import Shrink.Names (dTermToN)
import Shrink.Types (DTerm, NTerm, Tactic, safeTactics, tactics)

import Control.Arrow (second)
import Control.Monad (filterM, when, (>=>))
import Data.Either (rights)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
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
  unitTests <- filterM doesFileExist . fmap (unitTestDir </>) =<< listDirectory unitTestDir
  srcs <- mapM (fmap pack . readFile) unitTests
  let uplcs' =
        rights
          [ (name,) <$> (parseProgram name >=> (fmap Script . desugar . annDeBruijn) $ src)
          | (name, src) <- zip unitTests srcs
          ]
      uplcs = [(name, uplc) | (name, Script (Program _ _ uplc)) <- uplcs']
  return $
    testGroup "Unit tests" $
      (fullTest <$> uplcs)
        ++ ( do
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
  assert $ run (dTermToN uplc) ~= run (dTermToN uplc')
