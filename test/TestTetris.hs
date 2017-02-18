import Tetris
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

fullLineTest1 = TestCase $ assertEqual "empty fullLine" True (fullLine [])
fullLineTest2 = TestCase $ assertEqual "fullLine" False (fullLine [Nothing])
fullLineTest3 = TestCase $ assertEqual "fullLine" True (fullLine [Just Block{shape=J, moving=True, origin=True}])
fullLineTest4 = TestCase $ assertEqual "fullLine" False (fullLine [Just Block{shape=J, moving=True, origin=True}, Nothing])

main :: IO Counts
main = runTestTT $ TestList [fullLineTest1, fullLineTest2, fullLineTest3, fullLineTest4]
