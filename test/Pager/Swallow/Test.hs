module Pager.Swallow.Test
    (swallowSuite)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Pager.Swallow

swallowSuite :: TestTree
swallowSuite = testGroup "Swallow"
    [testCase "swallow test" testSwallow]

testSwallow :: Assertion
testSwallow = "something" @=? swallow "some" "thing"
