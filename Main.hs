module Main where

import           Control.Monad       (replicateM_, void)
import           Prelude             hiding (Left, Right)
import           System.Console.ANSI
import           System.IO

data Direction = Up | Down | Left | Right
data Mark = Vacant | O | X
  deriving (Eq)
type Cursor = Int
type Board = [Mark]
type GameState = (Board, Cursor, Mark)

instance Show Mark where
  show Vacant = " "
  show O      = "O"
  show X      = "X"

showBoard :: Board -> Cursor -> String
showBoard b c = concat $ zipWith (++) shownSquares
    ["|","|","\n--+--+--\n",
     "|","|","\n--+--+--\n",
     "|","|","\n"]
  where
    shownCursor = [if c == n then '>' else ' ' | n <- [0..8]]
    shownSquares = zipWith (:) shownCursor (map show b)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newX (x:xs) | n == 0 = newX:xs
                         | otherwise = x:replaceNth (n-1) newX xs


moveCursorUp    :: Cursor -> Cursor
moveCursorUp    c = (c - 3) `mod` 9

moveCursorDown  :: Cursor -> Cursor
moveCursorDown  c = (c + 3) `mod` 9

moveCursorLeft  :: Cursor -> Cursor
moveCursorLeft  c = (c - 1) `mod` 3 + ((c `quot` 3) * 3)

moveCursorRight :: Cursor -> Cursor
moveCursorRight c = (c + 1) `mod` 3 + ((c `quot` 3) * 3)


nextTurn :: Mark -> Mark
nextTurn O = X
nextTurn X = O
nextTurn x = x

placeMark :: GameState -> IO ()
placeMark gs@(b, c, t)
    | b !! c == Vacant = winCheck (replaceNth c t b, c, t)
    | otherwise = gameLoop gs


winCheck :: GameState -> IO ()
winCheck (b, c, t)
  | isWin t b = winScreen b t
  | isFull b  = winScreen b Vacant
  | otherwise = gameLoop (b, c, nextTurn t)
  where
    isWin :: Mark -> Board -> Bool
    isWin t b = any (all (== t)) $ map (map (b !!)) winSpans
      where
        winSpans :: [[Int]]
        winSpans = rows ++ cols ++ dias
          where ns = [0..2]
                ks = map (*3) ns
                rows = [[n + k | n <- ns] | k <- ks]
                cols = [[n + k | k <- ks] | n <- ns]
                dias = [zipWith (+) ns ks, zipWith (+) ns (reverse ks)]

    isFull :: Board -> Bool
    isFull = notElem Vacant


eraseLine :: IO ()
eraseLine = clearLine >> cursorUpLine 1

getKeyPress :: IO Char
getKeyPress = do
    k <- getChar
    cursorBackward 1
    clearFromCursorToLineEnd
    return k

printBoard :: Board -> Cursor -> IO ()
printBoard b c = replicateM_ 5 eraseLine >> putStr (showBoard b c)

printBoard1 :: Board -> IO ()
printBoard1 b = printBoard b (negate 1)

gameLoop :: GameState -> IO ()
gameLoop gs@(board, cursor, turn) = do
    printBoard board cursor
    putStr $ show turn
    putStr "'s turn."
    hFlush stdout
    munchNextInput gs

munchNextInput :: GameState -> IO ()
munchNextInput gs@(board, cursor, turn) = do
    k <- getKeyPress
    case k of
      'w' -> gameLoop (board, moveCursorUp    cursor, turn)
      'a' -> gameLoop (board, moveCursorLeft  cursor, turn)
      's' -> gameLoop (board, moveCursorDown  cursor, turn)
      'd' -> gameLoop (board, moveCursorRight cursor, turn)
      'e' -> placeMark gs
      'q' -> quitMessage
      _   -> munchNextInput gs

winScreen :: Board -> Mark -> IO ()
winScreen board who = do
    printBoard1 board
    if who == Vacant
      then putStrLn "It's a tie."
      else putStr (show who) >> putStrLn " won."
    isReplay <- askUser "New game?"
    if isReplay
      then clearLine >> putStr "\n" >> startNewGame
      else eraseLine >> quitMessage

askUser :: String -> IO Bool
askUser prompt = do
    putStr prompt
    putStr " [y/n]"
    hFlush stdout
    fadSnip
  where fadSnip = do k <- getKeyPress
                     case k of
                       'y' -> return True
                       'n' -> return False
                       _   -> fadSnip

quitMessage :: IO ()
quitMessage = putStrLn "\n\nGoodbye"

startNewGame :: IO ()
startNewGame = putStr "\n\n\n\n\n" >> gameLoop (replicate 9 Vacant, 4, X)

main :: IO ()
main = do
    originalBuffering <- hGetBuffering stdin
    hSetBuffering stdin NoBuffering
    hideCursor
    hSetEcho stdin False

    startNewGame

    hSetEcho stdin True
    showCursor
    hSetBuffering stdin originalBuffering
