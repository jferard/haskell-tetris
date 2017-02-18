import Tetris
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

fullLineTest1 = TestCase $ assertEqual "empty fullLine" True (fullLine ([]::[Maybe Int]))
fullLineTest2 = TestCase $ assertEqual "fullLine" False (fullLine [Nothing::Maybe Int])
fullLineTest3 = TestCase $ assertEqual "fullLine" True (fullLine [Just 1])
fullLineTest4 = TestCase $ assertEqual "fullLine" False (fullLine [Just 1, Nothing])

main :: IO Counts
main = runTestTT $ TestList [fullLineTest1, fullLineTest2, fullLineTest3, fullLineTest4]
