#!/usr/bin/env runhaskell
import Data.List (intercalate)

data Sqrare = Vacant | O | X
  deriving (Eq)

data GameState = GS [Sqrare] Int Sqrare

instance Show Sqrare where
  show Vacant = " "
  show O = "O"
  show X = "X"

interlace :: [a] -> [a] -> [a]
interlace [] _ = []
interlace _ [] = []
interlace (x:xs) (y:ys) = x:y:interlace xs ys

triplelace :: [a] -> [a] -> [a] -> [a]
triplelace [] _ _ = []
triplelace _ [] _ = []
triplelace _ _ [] = []
triplelace (x:xs) (y:ys) (z:zs) = x:y:z:triplelace xs ys zs

instance Show GameState where
  show (GS b c t) = (++ show t ++ "'s turn.") $ ("\n" ++) $ concat $ triplelace cusses (map show b) ["|","|","\n--+--+--\n","|","|","\n--+--+--\n","|","|","\n"]
    where cusses = map (\n -> if c == n then ">" else " ") [0..8]

-- 0 1 2
-- 3 4 5
-- 6 7 8

moveCurserUp :: GameState -> GameState
moveCurserUp    (GS b c t) = GS b ((c - 3) `mod` 9) t

moveCurserDown :: GameState -> GameState
moveCurserDown  (GS b c t) = GS b ((c + 3) `mod` 9) t

moveCurserLeft :: GameState -> GameState
moveCurserLeft  (GS b c t) = GS b ((c - 1) `mod` 3 + ((c `quot` 3) * 3)) t

moveCurserRight :: GameState -> GameState
moveCurserRight (GS b c t) = GS b ((c + 1) `mod` 3 + ((c `quot` 3) * 3)) t

placeMark :: GameState -> IO GameState
placeMark gs@(GS b c t)
    | b !! c == Vacant = return $ GS (overwriteAt b c t) c (flipTeam t)
    | otherwise = do putStrLn "you can not overwrite a filled square"; return gs
  where
    overwriteAt :: [a] -> Int -> a -> [a]
    overwriteAt xs n nx = (take n xs) ++ [nx] ++ (drop (n+1) xs) -- in c "xs[n] = nx"
    flipTeam :: Sqrare -> Sqrare
    flipTeam O = X
    flipTeam X = O
    flipTeam Vacant = Vacant

gameLoop :: GameState -> IO ()
gameLoop gs = do
  k <- getChar
  case k of  -- Check if proper for tail recusion
    'w' -> gameLoop $ moveCurserUp gs
    'a' -> gameLoop $ moveCurserLeft gs
    's' -> gameLoop $ moveCurserDown gs
    'd' -> gameLoop $ moveCurserRight gs
    'e' -> placeMark gs >>= gameLoop
    'q' -> do putStrLn "Goodbye"; return ()
    '\n'-> do print gs; gameLoop gs-- show (GS b c t) = concatMap show b
    _ -> gameLoop gs

main :: IO ()
main = do
  let newGame = GS [Vacant | x <- [1..9]] 4 X
  print newGame
  gameLoop newGame
