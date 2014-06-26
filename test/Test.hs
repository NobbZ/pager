module Main where

import           Test.Tasty             (TestTree, defaultMain, testGroup)

import           Pager.PaperFormat.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ paperFormatSuite
            ]
