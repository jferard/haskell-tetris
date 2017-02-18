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

--Returns an empty Tetris grid
newGame :: GridBlock
newGame = replicate gridHeight (replicate gridWidth Nothing)

--Returns a tuple containing a random shape and a generator
randomShape :: RandomGen g => g -> (Shape, g)
randomShape g = case randomR (0,length [J ..]-1) g of (r, g') -> (toEnum r, g')

--Updates the state of a Tetris grid by gravitating, clearing lines and
--stopping blocks
update :: GridBlock -> Shape -> GridBlock
update state = addBlock (gravitate (clearLines (freezeBlocks state)))

--Adds shaped blocks on top of the grid
addBlock :: GridBlock -> Shape -> GridBlock
addBlock rows shape | empty rows && not (gameOver rows) = createShape shape ++ tail (tail (tail (tail rows)))
                    | otherwise = rows

--Drops current shape to the bottom
dropBlock :: GridBlock -> GridBlock
dropBlock rows | gravitate rows /= rows = dropBlock (gravitate rows)
               | otherwise = rows

--Speeds up the gravity
speedUp :: GridBlock -> GridBlock
speedUp = gravitate

--Moves the moving blocks right
moveRight :: GridBlock -> GridBlock
moveRight rows | not(touchright rows) = transpose (gravitate (transpose rows))
               | otherwise = rows
        where
            touchright :: GridBlock -> Bool
            touchright rows = any moving (mapMaybe last rows)

--Moves the moving blocks left
moveLeft :: GridBlock -> GridBlock
moveLeft rows | not(touchleft rows) = map reverse (transpose (gravitate (transpose (map reverse rows))))
              | otherwise = rows
        where
            touchleft :: GridBlock -> Bool
            touchleft rows = any moving (mapMaybe head rows)

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

--Gives the score for current state
score :: Eq a => Grid a -> Int
score state = let num_full_lines = length (filter (==True) (map fullLine state)) in num_full_lines*num_full_lines

--Indicates whether the given states results in a game over
gameOver :: GridBlock -> Bool
gameOver state = any (not . all moving . catMaybes) (take 4 state)

---Helpers

gridHeight :: Int
gridHeight = 26

gridWidth:: Int
gridWidth = 10

--Gravitates moving blocks downwards
gravitate :: GridBlock -> GridBlock
gravitate rows | not(stopped rows) = transpose (gravitate_rows (transpose rows))
               | otherwise = rows
    where
        gravitate_row :: Row Block -> Row Block
        gravitate_row [] = []
        gravitate_row row | movingBlock (head row) = move_blocks row
        gravitate_row (h:t) = h : gravitate_row t

        gravitate_rows :: GridBlock -> GridBlock
        gravitate_rows [] = []
        gravitate_rows lis = gravitate_row (head lis) : gravitate_rows (tail lis)

        --Moves blocks downwards
        move_blocks :: Row Block -> Row Block
        move_blocks l | is_gap (gap l) = (Nothing:movingBlocks l) ++ tail (gap l) ++ ground l
            where
                is_gap :: Row Block -> Bool
                is_gap row = not (null (gap row)) && isNothing (head (gap row))

                movingBlocks :: Row Block -> Row Block
                movingBlocks (h:t) | movingBlock h = h:movingBlocks t
                movingBlocks _ = []

                gap:: Row Block -> Row Block
                gap (Nothing:t) = Nothing:gap' t
                    where
                        gap' (Nothing:t) = Nothing:gap' t
                        gap' _ = []

                gap (h:t) | movingBlock h = gap t
                gap _ = []

                ground :: Row Block -> Row Block
                ground [] = []
                ground (h:t) | stationaryBlock h = h:t
                             | otherwise = ground t

--Determines whether the moving blocks have stopped moving
stopped :: GridBlock -> Bool
stopped rows = any stopped' columns || empty rows
    where
        columns = transpose rows
        stopped' :: Row Block -> Bool
        stopped' [] = False
        stopped' column | all movingBlock column = True
        stopped' (first:second:_) | movingBlock first && stationaryBlock second = True
        stopped' (_:t) = stopped' t

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
freezeBlocks rows | stopped rows = map freezeBlocks' rows
                  | otherwise = rows
            where
                freezeBlocks' :: Row Block -> Row Block
                freezeBlocks' = map (fmap freeze) -- fmap freeze :: Maybe Block -> Maybe Block
                    where
                        freeze :: Block -> Block
                        freeze (Block s _ o) = Block s False o

--Creates a grid containing a given shape to put on top of a game grid
createShape :: Shape -> GridBlock
createShape sh | sh == I = pad createI
               | sh == J = pad createJ
               | sh == L = pad createL
               | sh == S = pad createS
               | sh == Z = pad createZ
               | sh == O = pad createO
               | sh == T = pad createT
        where
              block shape origin = Just (Block shape True origin)
              x = Nothing
              hpad l = replicate 3 x ++ l ++ replicate 4 x

              pad s | length s == 2 = [replicate 10 x] ++ map hpad s ++ [replicate 10 x]
                    | length s == 3 = replicate 10 x : map hpad s
                    | otherwise = map hpad s

              createI =
                    [
                        [x,b,x],
                        [x,o,x],
                        [x,b,x],
                        [x,b,x]
                    ]
                    where
                        b = block I False
                        o = block I True

              createJ =
                    [
                        [x,b,x],
                        [x,o,x],
                        [b,b,x]
                    ]
                    where
                        b = block J False
                        o = block J True

              createL =
                    [
                        [x,b,x],
                        [x,o,x],
                        [x,b,b]
                    ]
                    where
                        b = block L False
                        o = block L True

              createS =
                    [
                        [x,b,b],
                        [b,o,x]
                    ]
                    where
                        b = block S False
                        o = block S True

              createZ =
                    [
                        [b,b,x],
                        [x,o,b]
                    ]
                    where
                        b = block Z False
                        o = block Z True

              createO =
                    [
                        [x,b,b],
                        [x,b,b]
                    ]
                    where
                        b = block O False
                        o = block O True
              createT = 
                    [
                        [b,o,b],
                        [x,b,x]
                    ]
                    where
                        b = block T False
                        o = block T True

