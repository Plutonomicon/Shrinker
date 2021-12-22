module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Tactics (shrinkingTactics)
import UnitTests (makeUnitTests)

main :: IO ()
main = do
  unitTests <- makeUnitTests
  defaultMain $
    testGroup
      "Shrinker"
      [ shrinkingTactics
      , unitTests
      ]
