module TetrisCurses where
import UI.NCurses
import System.Random
import Control.Monad
import Data.Char
import Tetris

block :: String
block = " ."

gridX :: Integer
gridX = 50

gridY :: Integer
gridY = 4

rows :: Integer
rows = toInteger (length newGame - 4)

columns :: Integer
columns = toInteger (length (head newGame))

data Colors = Colors { gridcolor::ColorID,
                        red::ColorID, green::ColorID, blue::ColorID,
                        yellow::ColorID, cyan::ColorID, white::ColorID,
                        magenta::ColorID,
                        redtext::ColorID }

playGame :: IO ()
playGame = do
    g <- newStdGen
    runCurses $ do
        w <- defaultWindow
        gridcolor <- newColorID ColorBlue ColorDefault 1
        red <- newColorID ColorRed ColorRed 2
        green <- newColorID ColorGreen ColorGreen 3
        blue <- newColorID ColorBlue ColorBlue 4
        yellow <- newColorID ColorYellow ColorYellow 5
        cyan <- newColorID ColorCyan ColorCyan 6
        white <- newColorID ColorWhite ColorWhite 7
        magenta <- newColorID ColorMagenta ColorMagenta 8
        redtext <- newColorID ColorRed ColorDefault 9

        let colors = Colors gridcolor red green blue yellow cyan white magenta redtext

        setCursorMode CursorInvisible
        setEcho False
        game w colors g

game :: Window -> Colors -> StdGen -> Curses()
game w colors g = do
    updateWindow w $ drawGrid gridY gridX (gridcolor(colors))
    updateWindow w (levelMenu colors)
    updateWindow w (clearStats colors)
    render
    ev <- getEvent w Nothing
    case ev of
        Nothing -> game w colors g
        Just (EventCharacter c) | isNumber c -> updateScreen w colors newGame 0 g (digitToInt c)
                                | c == 'q' -> return ()
        Just _ -> game w colors g


updateScreen :: Window -> Colors -> Grid Block -> Int -> StdGen -> Int -> Curses()
updateScreen w colors gameState currentScore gen lvl = do
    updateWindow w $ do
        drawBlocks colors gameState
        drawScore colors currentScore
        drawLevel colors lvl
        when (gameOver gameState) (drawGameOver colors)
    render
    ev <- getEvent w (Just ((1+(9-toInteger lvl))*100))
    case ev of
        Nothing -> updateScreen w colors state newScore gen' lvl
        Just ev'
            | ev' == EventCharacter 'q' -> return ()
            | ev' == EventSpecialKey KeyLeftArrow
                -> updateScreen w colors (moveLeft state) newScore gen' lvl
            | ev' == EventSpecialKey KeyRightArrow
                -> updateScreen w colors (moveRight state) newScore gen' lvl
            | ev' == EventSpecialKey KeyDownArrow
                -> updateScreen w colors (speedUp state) newScore gen' lvl
            | ev' == EventSpecialKey KeyUpArrow
                -> updateScreen w colors (rotate state) newScore gen' lvl
            | ev' == EventCharacter ' '
                -> updateScreen w colors (dropBlock state) newScore gen' lvl
            | ev' == EventCharacter 'r'
                -> game w colors gen
            | otherwise -> updateScreen w colors state newScore gen' lvl
        where
            nextshape :: Shape
            nextshape = fst (randomShape gen)

            gen':: StdGen
            gen' = snd (randomShape gen)

            state :: Grid Block
            state = update gameState nextshape

            newScore :: Int
            newScore = currentScore + (score gameState*lvl)

drawGrid :: Integer -> Integer -> ColorID -> Update()
drawGrid y x c = do
    setColor c
    moveCursor y (x+2)
    drawString gridTop
    drawLines (y+1) (x+1)
    moveCursor (rows+y+1) (x+1)
    drawString gridBottom
    where
        gridTop :: String
        gridTop = "____________________"

        gridMiddle :: String
        gridMiddle = "|                    |"

        gridBottom :: String
        gridBottom = " -------------------- "

        drawLines :: Integer -> Integer -> Update()
        drawLines y x = drawLines' y x rows
            where
                drawLines' :: Integer -> Integer -> Integer -> Update()
                drawLines' y x n | n > 0 = do
                    moveCursor y x
                    drawString gridMiddle
                    drawLines' (y+1) x (n-1)
                                 | otherwise = return()


drawBlocks :: Colors -> Grid Block -> Update()
drawBlocks _ [] = return ()
drawBlocks colors (head:tail) | length (head:tail) <= fromIntegral rows = do
                            let y = (gridY+rows)- toInteger (length tail)
                            drawLine head y
                            drawBlocks colors tail
                       | otherwise = drawBlocks colors tail
    where
        drawLine :: Row Block -> Integer -> Update()
        drawLine [] y = return ()
        drawLine (head:tail) y = do
                let x = columns-(toInteger (length block)* toInteger (length tail))
                moveCursor y (gridX+x+columns)
                draw head
                drawLine tail y
            where
                drawBlock :: ColorID -> Update()
                drawBlock color = do
                    setColor color
                    drawString block

                draw :: Maybe Block -> Update()
                draw (Just (Block I _ _)) = drawBlock (red(colors))
                draw (Just (Block S _ _)) = drawBlock (green(colors))
                draw (Just (Block O _ _)) = drawBlock (blue(colors))
                draw (Just (Block T _ _)) = drawBlock (yellow(colors))
                draw (Just (Block Z _ _)) = drawBlock (cyan(colors))
                draw (Just (Block J _ _)) = drawBlock (white(colors))
                draw (Just (Block L _ _)) = drawBlock (magenta(colors))
                draw Nothing = drawBlock (gridcolor(colors))

drawGameOver :: Colors -> Update()
drawGameOver colors = do
    moveCursor (gridY + quot rows 2) (gridX + 8)
    setColor (redtext(colors))
    drawString "         "
    moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
    drawString "     GAME OVER!     "
    moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
    drawString " press 'r' to retry "

drawScore :: Colors -> Int -> Update()
drawScore colors score = do
    moveCursor (gridY - 1) (gridX + 1)
    setColor (gridcolor(colors))
    let scorestr = show score
    drawString ("Score: " ++ scorestr)

drawLevel :: Colors -> Int -> Update()
drawLevel colors level = do
    moveCursor (gridY - 1) (gridX + 15)
    setColor (gridcolor(colors))
    drawString ("Level: " ++ show level)

levelMenu :: Colors -> Update()
levelMenu colors = do
    setColor (redtext(colors))
    drawString "                    "
    moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
    drawString "    Choose level:   "
    moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
    drawString "        0-9         "

clearStats :: Colors -> Update()
clearStats colors = do
    moveCursor (gridY - 1) (gridX + 1)
    setColor (gridcolor(colors))
    drawString "                      "