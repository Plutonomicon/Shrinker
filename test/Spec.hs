module Main (main) where

import Test.Tasty (defaultMain, testGroup,localOption)
import Test.Tasty.Hedgehog(HedgehogTestLimit(HedgehogTestLimit))

import Tactics (shrinkingTactics)
import UnitTests (makeUnitTests)

import System.Environment(getEnv)

main :: IO ()
main = do
  getEnv "LANG" >>= print
  unitTests <- makeUnitTests
  defaultMain $ 
     testGroup
      "Shrinker"
      [ localOption (HedgehogTestLimit (Just 100)) shrinkingTactics
      -- ideally this would be at least 1000 but that seems to cause ram issues
      , localOption (HedgehogTestLimit (Just   1)) unitTests
      ]
