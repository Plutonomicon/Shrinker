module Main (main) where

import Test.Tasty

import Tactics(shrinkingTactics)
import UnitTests(makeUnitTests)

main :: IO ()
main = do
  unitTests <- makeUnitTests
  defaultMain $ testGroup "Shrinker"
    [shrinkingTactics 
    ,unitTests
    ]
