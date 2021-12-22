module Main (main) where

import Test.Tasty

import Tactics(shrinkingTactics)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Shrink"
    [shrinkingTactics ]
