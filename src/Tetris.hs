module Tetris(
    fullLine,
    newGame,
    randomShape,
    update,
    addBlock,
    dropBlock,
    speedUp,
    moveRight,
    moveLeft,
    rotate,
    score,
    gameOver,
    Grid,
    Row,
    Block(..),
    Shape(..)
) where

import Data.List
import Data.Maybe
import System.Random

data Shape = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

data Block = Block { shape :: Shape, moving::Bool, origin::Bool}
            deriving (Eq, Show)

type Row a  = [Maybe a]

type Grid a = [Row a]

type GridBlock = Grid Block

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
score :: Eq a => Grid a -> Int
score state = let num_full_lines = length (filter (==True) (map fullLine state)) in num_full_lines*num_full_lines

--Indicates whether the given states results in a game over
-- One of the first rows has a Just Block that is stationary
gameOver :: GridBlock -> Bool
gameOver state = any (any stationaryBlock) (take 4 state)

--Updates the state of a Tetris grid by gravitating, clearing lines and
--stopping blocks
update :: GridBlock -> Shape -> GridBlock
update state = addBlock (gravitate (clearLines (freezeBlocks state)))

--Adds shaped blocks on top of the grid
addBlock :: GridBlock -> Shape -> GridBlock
addBlock rows shape | empty rows && not (gameOver rows) = createShape shape ++ drop 4 rows -- the 4 first rows are invisible
                    | otherwise = rows

--Drops current shape to the bottom
dropBlock :: GridBlock -> GridBlock
dropBlock rows | gravitate rows /= rows = dropBlock (gravitate rows)
               | otherwise = rows

--rotates the moving blocks clockwise
rotate :: GridBlock -> GridBlock
rotate grid = insertRotated' (clearGrid grid) (rotateBlock grid) (map (getBlock grid) (movingCoordinates grid))
    where
        insertRotated':: GridBlock -> [(Int,Int)] -> [Maybe Block] -> GridBlock
        insertRotated' grid [] _ = grid
        insertRotated' grid (h:t) (val:valt) = insertRotated' (setBlock grid h val) t valt

        clearGrid :: GridBlock -> GridBlock
        clearGrid grid = clearGrid' grid (movingCoordinates grid)
            where
                clearGrid' :: GridBlock -> [(Int,Int)] -> GridBlock
                clearGrid' = foldl (\ grid h -> setBlock grid h Nothing)

        movingCoordinates :: GridBlock -> [(Int,Int)]
        movingCoordinates [] = []
        movingCoordinates (h:t) = movingCoordinates' h (25 - length t)  ++ movingCoordinates t
            where
                movingCoordinates' :: Row Block -> Int -> [(Int,Int)]
                movingCoordinates' [] _ = []
                movingCoordinates' (h:t) y | movingBlock h = (y,9- length t):movingCoordinates' t y
                                        | otherwise = movingCoordinates' t y

        getOrigin::GridBlock -> (Int,Int)
        getOrigin grid = head (origins grid)

        isOrigin:: GridBlock -> (Int,Int) -> Bool
        isOrigin grid (x,y) = isJust (getBlock grid (x,y)) && origin (fromJust (getBlock grid (x,y)))

        origins:: GridBlock -> [(Int,Int)]
        origins grid = filter (isOrigin grid) (movingCoordinates grid)

        rotateBlock:: GridBlock -> [(Int,Int)]
        rotateBlock grid | hasOrigin grid 
                        && all (unoccupied grid) (map (rotatePoint (getOrigin grid)) (movingCoordinates grid))
                        = map (rotatePoint (getOrigin grid)) (movingCoordinates grid)
                         | otherwise = movingCoordinates grid

        rotatePoint::(Int,Int) -> (Int,Int) -> (Int,Int)
        rotatePoint (originx,originy) (x,y) = (originx + originy - y, originy - originx + x)

        hasOrigin::Grid Block -> Bool
        hasOrigin grid = not (null (origins grid))

        unoccupied::Grid Block-> (Int,Int) -> Bool
        unoccupied grid (x,y) = (x > 0 && x < gridHeight && y > 0 && y < gridWidth) 
                     && not (stationaryBlock (getBlock grid (x,y)))

        getBlock :: Grid Block -> (Int,Int) -> Maybe Block
        getBlock grid (x,y) = (grid !! x) !! y

        setBlock :: Grid Block -> (Int,Int) -> Maybe Block -> Grid Block
        setBlock grid (x,y) val =
                fst (splitAt x grid) ++ setBlock' (head (snd(splitAt x grid))) y val:tail(snd (splitAt x grid))
            where
                setBlock' :: Row Block -> Int -> Maybe Block -> Row Block
                setBlock' row y val = fst (splitAt y row) ++ val:tail(snd (splitAt y row))

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
        moveRight' :: Row Block -> Row Block
        moveRight' row =
            move_blocks left_of_row moving_blocks gap border
                where
                    -- let's split the row in its interesting parts with span !
                    (left_of_row, moving_part_of_row) = span (not.movingBlock) row
                    (moving_blocks, gap_and_border) = span movingBlock moving_part_of_row
                    (gap, border) = span (==Nothing) gap_and_border

                    move_blocks :: Row Block->Row Block->Row Block->Row Block->Row Block
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
        stopped' :: Row Block -> Bool
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
        empty' :: Row Block -> Bool
        empty' l | not (any moving (catMaybes l)) = True
        empty' l = False

--Clears all full lines from the grid
clearLines :: GridBlock -> GridBlock
clearLines rows | empty rows = replicate (missing_rows rows) empty_row ++ remove_lines rows
                | otherwise = rows
        where
              missing_rows :: GridBlock -> Int
              missing_rows rows = length rows - length (remove_lines rows)

              empty_row :: Row Block
              empty_row = replicate 10 Nothing

              remove_lines :: GridBlock -> GridBlock
              remove_lines = filter (not . fullLine)

--Determines whether a line is full
fullLine :: Eq a => Row a -> Bool
fullLine = all (/= Nothing)

--Changes moving blocks that have stopped moving to stationary
freezeBlocks :: GridBlock -> GridBlock
freezeBlocks rows | stoppedRight (transpose rows) = map freezeBlocks' rows
                  | otherwise = rows
            where
                freezeBlocks' :: Row Block -> Row Block
                freezeBlocks' = map (fmap freeze) -- fmap freeze :: Maybe Block -> Maybe Block
                freeze :: Block -> Block
                freeze (Block s _ o) = Block s False o

--Creates a grid containing a given shape to put on top of a game grid
createShape :: Shape -> GridBlock
createShape sh = pad (create sh)
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

