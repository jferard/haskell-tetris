module Tetris(
    fullLine,
    newGame,
    randomShape,
    update,
    addTetromino,
    dropBlock,
    speedUp,
    moveRight,
    moveLeft,
    rotate,
    score,
    gameOver,
    GridBlock,
    RowBlock,
    Block(..),
    Shape(..)
) where

import Data.List
import Data.Either
import Data.Maybe
import System.Random
import Debug.Trace
import Grid

data Shape = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

-- a block is defined by the shape it belongs to, whether it is moving or not and whether it is the origin of the shape or not
data Block = Block { shape :: Shape, moving::Bool, origin::Bool}
            deriving (Eq, Show)

type GridBlock = Grid (Maybe Block)
type RowBlock = Row (Maybe Block)

type Tetromino = Grid (Maybe Block)

---Helpers

gridHeight :: Int
gridHeight = 26

gridWidth:: Int
gridWidth = 10


--Returns an empty Tetris grid
newGame :: GridBlock
newGame = replicate gridHeight (replicate gridWidth Nothing)

--Returns a tuple containing a random shape and a generator
randomShape :: RandomGen g => g -> (Shape, g)
randomShape g = let (r, g') = randomR (0,length [J ..]-1) g in (toEnum r, g')

-- a (infinite) list of random shapes
randomShapes gen = let (s, gen') = randomShape gen in s:randomShapes gen'

--Gives the score for current state
score :: Eq a => Grid (Maybe a) -> Int
score state = let num_full_lines = length (filter (==True) (map fullLine state)) in num_full_lines*num_full_lines

--Indicates whether the given states results in a game over
-- One of the first rows has a Just Block that is stationary
gameOver :: GridBlock -> Bool
gameOver state = any (any stationaryBlock) (take 4 state)

--Updates the state of a Tetris grid by gravitating, clearing lines and
--stopping blocks
update :: GridBlock -> Shape -> GridBlock
update state = addTetromino (gravitate (clearLines (freezeBlocks state)))

--Adds shaped blocks on top of the grid
addTetromino :: GridBlock -> Shape -> GridBlock
addTetromino rows shape | empty rows && not (gameOver rows) = createTetromino shape ++ drop 4 rows -- the 4 first rows are invisible
                    | otherwise = rows

--Drops current shape to the bottom
dropBlock :: GridBlock -> GridBlock
dropBlock rows | gravitate rows /= rows = dropBlock (gravitate rows)
               | otherwise = rows

--rotates the moving blocks clockwise
rotate :: GridBlock -> GridBlock
rotate grid = case setCellsInGrid (clearMovingFromGrid grid) (coordsAfterMovingRotation grid) movingBlocks of
                    Left _ -> trace ("rotation out of field!") grid
                    Right g -> g
    where
        -- first step: remove all moving blocks from grid
        clearMovingFromGrid :: GridBlock -> GridBlock
        clearMovingFromGrid grid = case clearCells grid (movingCoordinates grid) of
            Left _ -> trace ("this can't happen!") grid
            Right g -> g

        -- second step: get all moving coordinates after rotation around the origin
        coordsAfterMovingRotation :: GridBlock -> [(Int,Int)]
        coordsAfterMovingRotation grid =
            case origins of
                (origin:_) | all unoccupied targetCoordinates -> targetCoordinates
                    where
                        targetCoordinates :: [(Int, Int)]
                        targetCoordinates = map (rotatePoint origin) (movingCoordinates grid)

                        rotatePoint::(Int,Int) -> (Int,Int) -> (Int,Int)
                        rotatePoint (originr, originc) (r, c) = (originr + originc - c, originc - originr + r)

                        unoccupied::(Int,Int) -> Bool
                        unoccupied (r, c) = case getCellInGrid grid r c of
                            Right b -> not (stationaryBlock b)
                            _ -> False

                _ -> trace ("no origin!!") (movingCoordinates grid)
            where
                origins:: [(Int,Int)]
                origins = filter isOrigin (movingCoordinates grid)

                isOrigin::(Int,Int) -> Bool
                isOrigin (r, c) = case getCellInGrid grid r c of
                    Right (Just Block{ origin = True }) -> True
                    _ -> False


        -- third step: get the list of moving blocks
        movingBlocks::[Maybe Block]
        movingBlocks = rights eitherMovingBlocks
            where
                eitherMovingBlocks :: [Either String (Maybe Block)]
                eitherMovingBlocks = map (uncurry(getCellInGrid grid)) (movingCoordinates grid)

        -- get a list of moving block coordinates, (r, c)
        movingCoordinates :: GridBlock -> [(Int,Int)]
        movingCoordinates = fst . (foldl (\ (acc, r) row -> (acc ++ movingCoordinatesInRow r row, r+1)) ([], 0))
            where
                movingCoordinatesInRow :: Int -> RowBlock -> [(Int,Int)]
                movingCoordinatesInRow r = fst . (foldl (\ (acc, c) b -> (if movingBlock b then acc ++ [(r, c)] else acc, c+1)) ([], 0))

--Speeds up the gravity
speedUp :: GridBlock -> GridBlock
speedUp = gravitate

--Moves the moving blocks left
moveLeft :: GridBlock -> GridBlock
moveLeft rows = map reverse (moveRight (map reverse rows))


--Gravitates moving blocks downwards
moveRight :: GridBlock -> GridBlock
moveRight rows | not(stoppedRight rows) = map moveRight' rows
               | otherwise = rows
    where
        moveRight' :: RowBlock -> RowBlock
        moveRight' row =
            move_blocks left_of_row moving_blocks gap border
                where
                    -- let's split the row in its interesting parts with span !
                    (left_of_row, moving_part_of_row) = span (not.movingBlock) row
                    (moving_blocks, gap_and_border) = span movingBlock moving_part_of_row
                    (gap, border) = span (==Nothing) gap_and_border

                    move_blocks :: RowBlock -> RowBlock -> RowBlock -> RowBlock -> RowBlock
                    move_blocks left_of_row moving_blocks (Nothing:gs) border =
                        left_of_row ++ [Nothing] ++ moving_blocks ++ gs ++ border
                    move_blocks _ _ _ _ = row -- no gap, nothing to move, ...

--Determines whether the moving blocks have stopped moving
stoppedRight :: GridBlock -> Bool
stoppedRight rows = any stopped' rows || empty rows
    where
        couples :: [a] -> [(a, a)]
        couples (x:y:ys) = scanl (\c z -> (snd(c), z)) (x, y) ys
        couples _ = []
        stopped' :: RowBlock -> Bool
        stopped' row | movingBlock (last row) = True
        stopped' row = any (\(first, second) -> movingBlock first && stationaryBlock second) (couples row)


--Gravitates moving blocks downwards
gravitate :: GridBlock -> GridBlock
gravitate rows | not(stoppedRight columns) = transpose (moveRight columns)
               | otherwise = rows
    where
        columns = transpose rows

--Determines whether a given block is moving
movingBlock :: Maybe Block -> Bool
movingBlock (Just Block {moving=True}) = True
movingBlock _ = False


--Determines whether a given block is moving
stationaryBlock :: Maybe Block -> Bool
stationaryBlock (Just Block {moving=False}) = True
stationaryBlock _ = False

--Determines whether there are no moving blocks
empty :: GridBlock -> Bool
empty rows = all empty' (transpose rows)
    where
        empty' :: RowBlock -> Bool
        empty' l | not (any moving (catMaybes l)) = True
        empty' l = False

--Clears all full lines from the grid
clearLines :: GridBlock -> GridBlock
clearLines rows | empty rows = replicate (missing_rows rows) (replicate gridWidth Nothing) ++ remove_lines rows
                | otherwise = rows
        where
              missing_rows :: GridBlock -> Int
              missing_rows rows = length rows - length (remove_lines rows)

              empty_row :: RowBlock
              empty_row = replicate gridWidth Nothing

              remove_lines :: GridBlock -> GridBlock
              remove_lines = filter (not . fullLine)

--Changes moving blocks that have stopped moving to stationary
freezeBlocks :: GridBlock -> GridBlock
freezeBlocks rows | stoppedRight (transpose rows) = map freezeBlocks' rows
                  | otherwise = rows
            where
                freezeBlocks' :: RowBlock -> RowBlock
                freezeBlocks' = map (fmap freeze) -- fmap freeze :: Maybe Block -> Maybe Block
                freeze :: Block -> Block
                freeze (Block s _ o) = Block s False o

--Creates a grid containing a given shape to put on top of a game grid
createTetromino :: Shape -> Tetromino
createTetromino sh = pad (create sh)
        where
              x = Nothing
              pad :: [[Maybe Block]] -> [[Maybe Block]]
              pad s | length s == 2 = blank_row : (hpadded ++ [blank_row])
                    | length s == 3 = blank_row : hpadded
                    | otherwise = hpadded
                        where
                            hpadded = map (hpad w) s
                            w = (length.head) s
                            blank_row = replicate gridWidth x
              hpad :: Int -> [Maybe Block] -> [Maybe Block]
              hpad w l = replicate before_count x ++ l ++ replicate after_count x
                        where
                            before_count::Int
                            before_count = (gridWidth - w) `div` 2
                            after_count::Int
                            after_count = gridWidth - w - before_count

              create :: Shape -> [[Maybe Block]]
              create sh = case sh of
                I ->
                    [
                        [x,b,x],
                        [x,o,x],
                        [x,b,x],
                        [x,b,x]
                    ]
                J ->
                    [
                        [x,b,x],
                        [x,o,x],
                        [b,b,x]
                    ]
                L ->
                    [
                        [x,b,x],
                        [x,o,x],
                        [x,b,b]
                    ]
                S ->
                    [
                        [x,b,b],
                        [b,o,x]
                    ]
                Z ->
                    [
                        [b,b,x],
                        [x,o,b]
                    ]
                O ->
                    [
                        [x,b,b],
                        [x,b,b]
                    ]
                T ->
                    [
                        [b,o,b],
                        [x,b,x]
                    ]
                where
                    b = Just (Block sh True False)
                    o = Just (Block sh True True)

