module Main where

import           Control.Monad       (replicateM_)
import           Prelude             hiding (Left, Right)
import           System.Console.ANSI
import           System.IO

data Direction = Up | Down | Left | Right

data Mark = Vacant | O | X
  deriving (Eq)

data GameState = InPlay [Mark] Int Mark | Won [Mark] Mark | Quit

instance Show Mark where
  show Vacant = " "
  show O      = "O"
  show X      = "X"

showBoard :: [Mark] -> Int -> String
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


moveCursor :: Direction -> GameState -> GameState
moveCursor d (InPlay b c t) = InPlay b nc t
  where nc = case d of
              Up    -> (c - 3) `mod` 9
              Down  -> (c + 3) `mod` 9
              Left  -> (c - 1) `mod` 3 + ((c `quot` 3) * 3)
              Right -> (c + 1) `mod` 3 + ((c `quot` 3) * 3)

placeMark :: GameState -> GameState
placeMark gs@(InPlay b c t)
    | b !! c == Vacant = nextTurn $ winCheck $ InPlay (replaceNth c t b) c t
    | otherwise = gs

quitGame :: GameState -> GameState
quitGame _ = Quit

winCheck :: GameState -> GameState
winCheck gs@(InPlay b _ t)
  | isWin t b = Won b t
  | isFull b  = Won b Vacant
  | otherwise = gs
  where
    isWin :: Mark -> [Mark] -> Bool
    isWin t b = any (all (== t)) $ map (map (b !!)) winSpans
      where
        winSpans :: [[Int]]
        winSpans = rows ++ cols ++ dias
          where ns = [0..2]
                ks = map (*3) ns
                rows = [[n + k | n <- ns] | k <- ks]
                cols = [[n + k | k <- ks] | n <- ns]
                dias = [zipWith (+) ns ks, zipWith (+) ns (reverse ks)]

    isFull :: [Mark] -> Bool
    isFull = notElem Vacant

nextTurn :: GameState -> GameState
nextTurn (InPlay b c O) = InPlay b c X
nextTurn (InPlay b c X) = InPlay b c O
nextTurn x              = x


eraseLine :: IO ()
eraseLine = clearLine >> cursorUpLine 1

getKeyPress :: IO Char
getKeyPress = do k <- getChar
                 cursorBackward 1
                 clearFromCursorToLineEnd
                 return k

printBoard :: [Mark] -> Int -> IO ()
printBoard b c = replicateM_ 5 eraseLine >> putStr (showBoard b c)

printBoard1 :: [Mark] -> IO ()
printBoard1 b = printBoard b (negate 1)

gameLoop :: GameState -> IO ()
gameLoop gs@(InPlay b c t) = do
    printBoard b c
    putStr $ show t
    putStr "'s turn."
    hFlush stdout

    stateTransform <- getStateTransition
    gameLoop $ stateTransform gs
  where
    getStateTransition = do k <- getKeyPress
                            case k of
                              'w' -> return $ moveCursor Up
                              'a' -> return $ moveCursor Left
                              's' -> return $ moveCursor Down
                              'd' -> return $ moveCursor Right
                              'e' -> return placeMark
                              'q' -> return quitGame
                              _   -> getStateTransition

gameLoop gs@(Won b t) = do
    printBoard1 b
    if t == Vacant
      then putStrLn "It's a tie."
      else putStr (show t) >> putStrLn " won."
    putStr "New game? [y/n]"
    hFlush stdout
    promtUser
  where promtUser = do k <- getKeyPress
                       case k of
                         'y' -> clearLine >> putStr "\n" >> startNewGame
                         'n' -> eraseLine >> gameLoop Quit
                         _   -> promtUser

gameLoop Quit = do
  putStrLn "\n\nGoodbye"
  return ()

startNewGame :: IO ()
startNewGame = putStr "\n\n\n\n\n" >> gameLoop (InPlay (replicate 9 Vacant) 4 X)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hideCursor
  startNewGame
  showCursor
