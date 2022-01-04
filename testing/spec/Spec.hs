module Main (main) where

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Shrink.Testing.Tactics (shrinkingTactics)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "shrinker tests"
      [ localOption (HedgehogTestLimit (Just 1000)) shrinkingTactics
      ]
