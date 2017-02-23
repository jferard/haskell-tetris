import Grid
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

fullLineTests = [
    TestCase $ assertEqual "empty fullLine" True (fullLine ([]::[Maybe Int])),
    TestCase $ assertEqual "fullLine" False (fullLine [Nothing::Maybe Int]),
    TestCase $ assertEqual "fullLine" True (fullLine [Just 1]),
    TestCase $ assertEqual "fullLine" False (fullLine [Just 1, Nothing]) ]

grid :: Grid (Maybe Int)
grid = [[Just 1, Just 2], [Just 3, Just 4], [Nothing, Just 5]]
f (Just x) = x >= 4
f _ = False
filterTest = TestCase $ assertEqual "empty" [] (filterCoordinates f grid)

grid2 :: Grid Int
grid2 = [map (\ x -> r*10 + x) [0..9] | r<-[0..9]]

gridTests = [
    TestCase $ assertEqual "grid" (Right 55) (getCellInGrid grid2 5 5),
    TestCase $ assertEqual "grid" (Right 55) (getCellInGrid grid2 (-1) 5),
    TestCase $ assertEqual "grid" (Right 55) (getCellInGrid grid2 5 (-1)),
    TestCase $ assertEqual "grid" (Right 55) (getCellInGrid grid2 (-1) 5),
    TestCase $ assertEqual "grid" (Right 55) (getCellInGrid grid2 5 50),
    TestCase $ assertEqual "grid" (Right grid2) (setCellInGrid grid2 5 5 (-10)),
    TestCase $ assertEqual "grid" (Right grid2) (setCellInGrid grid2 (-1) 5 (-10)),
    TestCase $ assertEqual "grid" (Right grid2) (setCellInGrid grid2 5 (-1) (-10)),
    TestCase $ assertEqual "grid" (Right grid2) (setCellInGrid grid2 (-1) 5 (-10)),
    TestCase $ assertEqual "grid" (Right grid2) (setCellInGrid grid2 5 50 (-10)) ]

main :: IO Counts
main = runTestTT $ TestList (fullLineTests ++ gridTests ++ [filterTest])
