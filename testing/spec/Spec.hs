module Main (main) where

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import UnitTests (makeUnitTests)
import Tactics (shrinkingTactics)

main :: IO ()
main = do
  unitTests <- makeUnitTests
  defaultMain $
    testGroup
      "shrinker tests"
      [ localOption (HedgehogTestLimit (Just 1000)) shrinkingTactics
      , localOption (HedgehogTestLimit (Just 1)) unitTests 
      ]
