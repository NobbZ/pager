module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import Pager.Swallow.Test
import Pager.Coconut.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ swallowSuite
            , coconutSuite
            ]
