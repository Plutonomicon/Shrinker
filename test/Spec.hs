module Main (main) where

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Tactics (shrinkingTactics)
import UnitTests (makeUnitTests)

main :: IO ()
main = do
  unitTests <- makeUnitTests
  defaultMain $
    testGroup
      "Shrinker"
      [ -- ideally this would be at least 1000 but that seems to cause ram issues
        localOption (HedgehogTestLimit (Just 1000)) shrinkingTactics
      , localOption (HedgehogTestLimit (Just 1)) unitTests
      ]
