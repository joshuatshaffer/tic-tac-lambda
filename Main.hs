module Main where

import           Control.Monad       (replicateM_)
import           System.Console.ANSI
import           System.IO

-- Type and Data Definitions --

data Mark = Vacant | O | X
  deriving (Eq)
type Cursor = Int
type Board = ([Mark], Int)
type GameState = (Board, Cursor, Mark)

-- Basic Functions --

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newX (x:xs) | n == 0 = newX:xs
                         | otherwise = x:replaceNth (n-1) newX xs

moveCursorUp, moveCursorDown, moveCursorLeft, moveCursorRight :: Int -> Cursor -> Cursor
moveCursorUp    s c = (c - s) `mod` (s*s)
moveCursorDown  s c = (c + s) `mod` (s*s)
moveCursorLeft  s c = (c - 1) `mod` s + ((c `quot` s) * s)
moveCursorRight s c = (c + 1) `mod` s + ((c `quot` s) * s)

nextTurn :: Mark -> Mark
nextTurn O = X
nextTurn X = O
nextTurn x = x

isFull :: Board -> Bool
isFull (ms, _) = Vacant `notElem` ms

isWin :: Mark -> Board -> Bool
isWin t (ms, s) = any (all (== t)) $ map (map (ms !!)) winSpans
  where
    winSpans :: [[Int]]
    winSpans = rows ++ cols ++ dias
      where ns = [0..(s-1)]
            ks = map (*s) ns
            rows = [[n + k | n <- ns] | k <- ks]
            cols = [[n + k | k <- ks] | n <- ns]
            dias = [zipWith (+) ns ks, zipWith (+) ns (reverse ks)]

-- Show Functions --

instance Show Mark where
  show Vacant = " "
  show O      = "O"
  show X      = "X"

showBoard :: Board -> Cursor -> String
showBoard (ms, s) c = concat $ zipWith (++) shownSquares looink
  where
    shownCursor = [if c == n then '>' else ' ' | n <- [0..(s*s-1)]]
    shownSquares = zipWith (:) shownCursor (map show ms)
    looink = (iterate ((bap++[stap])++) (bap ++ ["\n"])) !! (s-1)
    bap = replicate (s-1) "|"
    stap = '\n' : ((iterate ("--+"++) "--\n") !! (s-1))

-- Output/Rendering --

eraseLine :: IO ()
eraseLine = clearLine >> cursorUpLine 1

printBoard :: Board -> Cursor -> IO ()
printBoard b@(_, s) c = replicateM_ (s*2-1) eraseLine >> putStr (showBoard b c)

printBoard1 :: Board -> IO ()
printBoard1 b = printBoard b (negate 1)

-- Input Helpers --

askUser :: String -> IO Bool
askUser prompt = do
    putStr prompt
    putStr " [y/n]"
    hFlush stdout
    fadSnip
  where fadSnip = do k <- getChar
                     case k of
                       'y' -> return True
                       'n' -> return False
                       _   -> fadSnip

-- Game Execution and Control Flow --

gameLoop :: GameState -> IO ()
gameLoop gs@(board, cursor, turn) = do
    printBoard board cursor
    putStr $ show turn
    putStr "'s turn."
    hFlush stdout
    munchNextInput gs

munchNextInput :: GameState -> IO ()
munchNextInput gs@(board@(_, s), cursor, turn) = do
    k <- getChar
    case k of
      'w' -> gameLoop (board, moveCursorUp    s cursor, turn)
      'a' -> gameLoop (board, moveCursorLeft  s cursor, turn)
      's' -> gameLoop (board, moveCursorDown  s cursor, turn)
      'd' -> gameLoop (board, moveCursorRight s cursor, turn)
      'e' -> placeMark gs
      'q' -> quitGame
      _   -> munchNextInput gs

placeMark :: GameState -> IO ()
placeMark gs@(b@(ms, s), c, t)
    | ms !! c == Vacant = winCheck ((replaceNth c t ms, s), c, t)
    | otherwise = gameLoop gs

winCheck :: GameState -> IO ()
winCheck (b, c, t)
    | isWin t b = winScreen b t
    | isFull b  = winScreen b Vacant
    | otherwise = gameLoop (b, c, nextTurn t)

winScreen :: Board -> Mark -> IO ()
winScreen board who = do
    printBoard1 board
    if who == Vacant
      then putStrLn "It's a tie."
      else putStr (show who) >> putStrLn " won."
    isReplay <- askUser "New game?"
    if isReplay
      then clearLine >> putChar '\n' >> startNewGame
      else eraseLine >> quitGame

quitGame :: IO ()
quitGame = putStrLn "\n\nGoodbye"

startNewGame :: IO ()
startNewGame = do
    hSetEcho stdin True
    showCursor
    hSetBuffering stdin LineBuffering

    putStr "What size board do you whant to play? (default 3): "
    hFlush stdout
    s <- ( getLine >>= (\l -> return $ (read l :: Int)))

    hSetBuffering stdin NoBuffering
    hideCursor
    hSetEcho stdin False

    putStr (replicate (s*2-1) '\n')
    gameLoop ((replicate (s*s) Vacant, s), 0, X)

main :: IO ()
main = do
    startNewGame

    hSetEcho stdin True
    showCursor
    hSetBuffering stdin LineBuffering
