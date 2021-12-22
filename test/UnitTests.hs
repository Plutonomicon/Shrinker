module UnitTests where

import Tactics

import Shrink
import Shrink.Names
import Shrink.Types 

import Control.Arrow               (second)
import Control.Monad               (filterM, when, (>=>))
import Data.Either                 (rights)
import Data.Functor                ((<&>))
import Data.List                   (isInfixOf)
import Data.Text                   (pack)
import Hedgehog                    (MonadTest, annotate, failure, success,property,assert)
import System.Directory            (doesFileExist, listDirectory)
import System.FilePath             ((</>))
import Test.Tasty                  (TestTree,testGroup)
import Test.Tasty.Hedgehog         (testProperty)
import UntypedPlutusCore.Core.Type (Program(Program)) 


import Plutus.V1.Ledger.Scripts         (Script (..))
import PlutusCore.Assembler.AnnDeBruijn (annDeBruijn)
import PlutusCore.Assembler.Assemble    (parseProgram)
import PlutusCore.Assembler.Desugar     (desugar)


data TacticType = Safe | Unsafe deriving Show

makeUnitTests :: IO TestTree
makeUnitTests = do
  unitTests <-  filterM doesFileExist . fmap ("./unitTests" </>) =<< listDirectory "./unitTests"
  srcs      <-  mapM (fmap pack . readFile) unitTests
  let uplcs' = rights [ (name,) <$> ( parseProgram name >=> (fmap Script . desugar . annDeBruijn) $ src )
                      | (name,src) <- zip unitTests srcs ]
      uplcs  = [(name,uplc) | (name,Script (Program _ _ uplc)) <- uplcs' ]
  return $ testGroup "Unit tests" $ (fullTest <$> uplcs) ++ ( do
    (name,uplc) <- uplcs
    (tactName,tact) <- [Safe,Unsafe] >>= \case
        Unsafe -> tactics     defaultShrinkParams
        Safe   -> safeTactics defaultShrinkParams <&> second (return .)
    return $ testProperty ("testing " ++ tactName ++ " on " ++ name) . property $ do
      when (('/':tactName) `isInfixOf` name) $ testNonTrivial tact (dTermToN uplc)
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
                             (_:_) -> success
fullTest :: (String,DTerm) -> TestTree
fullTest (name,uplc) = testProperty ("full test of shrink on " ++ name) . property $ do
  let uplc' = shrinkDTerm uplc
  assert $ run (dTermToN uplc) ~= run (dTermToN uplc')
