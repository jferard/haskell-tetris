module Grid (
    Row,
    Grid,
    clearCells,
    getRow,
    getCell,
    getCellInGrid,
    setRow,
    setCell,
    setCellInGrid,
    setCellsInGrid,
    filterCoordinates,
    fullLine
) where

import Data.List
import Data.Maybe
import Control.Monad

type Row a =  [a]
type Grid a =  [Row a]

getRow :: Grid a -> Int -> Either String (Row a)
getRow grid r
    | r < 0 || r >= length grid = Left ("No row at index "++show r)
    | otherwise = Right (grid !! r)

getCell :: Row a -> Int -> Either String a
getCell row c
    | c < 0 || c >= length row = Left ("No cell at index "++show c)
    | otherwise = Right (row !! c)

getCellInGrid :: Grid a -> Int -> Int-> Either String a
getCellInGrid grid r c = getRow grid r >>= (flip getCell) c

setRow :: Grid a -> Int -> Row a -> Either String (Grid a)
setRow grid r row
    | r < 0 || r >= length grid = Left ("No row at index "++show r)
    | otherwise = Right (rowsBefore ++ row : tail(rowsAfter))
        where (rowsBefore, rowsAfter) = splitAt r grid

setCell :: Row a -> Int -> a -> Either String (Row a)
setCell row c val
    | c < 0 || c >= length row = Left ("No cell at index "++show c)
    | otherwise = Right (cellsBefore ++ val : tail(cellsAfter))
        where (cellsBefore, cellsAfter) = splitAt c row

setCellInGrid :: Grid a -> Int -> Int -> a -> Either String (Grid a)
setCellInGrid grid r c val = getRow grid r >>= (\ row -> setCell row c val) >>= setRow grid r

-- get a list of coordinates (r, c) for elements fulfilling a condition
filterCoordinates :: (a -> Bool) -> Grid a -> [(Int,Int)]
filterCoordinates predicate = fst . (
        foldl (\ (acc, r) row -> (acc ++ map (\ c -> (r, c)) (filterCoordinatesInRow predicate row), r+1)) ([], 0)
    )

filterCoordinatesInRow :: (a -> Bool) -> Row a ->  [Int]
filterCoordinatesInRow predicate = fst . (foldl (\ (acc, c) b -> (if predicate b then acc ++ [c] else acc, c+1)) ([], 0))

-- set multiple cells in grid
setCellsInGrid :: Grid a -> [(Int,Int)] -> [a] -> Either String (Grid a)
setCellsInGrid grid coords values = foldM (\ g ((r, c), value) -> setCellInGrid g r c value) grid (zip coords values)

-- Maybes

clearCell :: Grid (Maybe a) -> Int -> Int -> Either String (Grid (Maybe a))
clearCell grid r c = setCellInGrid grid r c Nothing

clearCells :: Grid (Maybe a) -> [(Int,Int)] -> Either String (Grid (Maybe a))
clearCells = foldM (\ acc (r, c) -> clearCell acc r c)

--Determines whether a line is full
fullLine :: Eq a => Row (Maybe a) -> Bool
fullLine = all (/= Nothing)

