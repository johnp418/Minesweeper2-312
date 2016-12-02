
{- Minesweeper V2 
  Developed by Adam Magdurulan, John Park, & Theodore Lau.
  In conjunction with UBC CPSC 312 course curriculum.
  Project #2 of the UBC CPSC 312 Winter 2016T1.
-}

-- TO DEVELOPERS: This project is accompanied by a README, please refer to it for 
--                outstanding issues/changes/commants during the course of development.

module Minesweeper2() where

import Data.List ( (\\) )
import System.IO
import System.Random

------------------------------
-- Board Class + Tile Class --
------------------------------
data Board = Board {
  tiles :: [Tile],
  height :: Int,
  width :: Int
}

instance Show Board where
  show b = showHeader (width b) ++ showTiles (tiles b) 1 where
    alignHeader n
      | n < 10   = " " ++ show n ++ " "
      | n >= 10  = show n ++ " "
    alignTiles n
      | n < 10   = "  " ++ show n ++ " "
      | n >= 10  = " " ++ show n ++ " "
    showHeader n = "     " ++ foldl (\a b -> a ++ alignHeader b) "" [1..n] ++ "\n     " ++ foldl (\a b -> a ++ " v ") "" [1..n] ++ "\n"
    showTiles [] _ = ""
    showTiles ts n = foldl (++) (alignTiles n ++ ">") (map show thisRow) ++ "\n" ++ showTiles otherRows (n+1) where
      (thisRow, otherRows) = splitAt (width b) ts

data Tile = Tile {
  value :: Int,
  hidden :: Bool,
  marked :: Bool,
  question :: Bool
} 

instance Show Tile where
  show Tile { question = True } = " ? "
  show Tile { marked = True } = " ! "
  show Tile { hidden = True } = " _ "
  show Tile { value = -1 } = " X "
  show Tile { value = 0 } = "   "
  show Tile { value = x } = " " ++ show x ++ " "

newTile :: Tile
newTile = Tile { value = 0, hidden = True, marked = False, question = False }
newMine :: Tile
newMine = Tile { value = -1, hidden = True, marked = False, question = False }

--------------------------------------------
-- Implementation code to build the grid. --
--------------------------------------------
buildGrid :: Int -> Int -> Int -> Board
buildGrid w h m = (mineGrid (blankGrid w h) m)

blankGrid :: Int -> Int -> Board
blankGrid w h = Board { tiles = [newTile | i <- [1..w], j <- [1..h]], width = w, height = h }

mineGrid :: Board -> Int -> Board
mineGrid b m = let 
  h = height b
  w = width b
  -- Place Mines
  place t 


                     -- -- Reservoir polling
                     -- place t      _   0 = t
                     -- place (t:ts) rng n
                     --    | rngValue <= fromIntegral n / fromIntegral (length (t:ts))  = newBomb : place ts rng' (n - 1)
                     --    | otherwise              = t : place ts rng' n
                     --    where (rngValue, rng') = randomR (0, 1) rng :: (Double, StdGen)
                         
                     -- -- DOOO EEEEEEIT
                     -- t' = place (tiles b) g n
                     --in
                     -- Board t' h w

--------------------------------------
-- Implementation code to run game. --
--------------------------------------

--markTile :: Tile -> Tile 
--markTile (Tile v m h _) = Tile v (not m) h False

--revealTile :: Tile -> Tile
--revealTile (Tile v m _ q) = Tile v m False q

--queryTile :: Tile -> Tile 
--queryTile (Tile v _ h q) = Tile v False h (not q)

--help :: 
--updateBoardAtN :: (Tile -> Tile) -> Board -> Int -> Board 
--updateBoardAtN f (Board t h w) n = Board (hlpr f t n) h w where 
--  hlpr _ [] _ = []
--  hlpr f (t:ts) 0 = f t:ts 
--  hlpr f (t:ts) n = t:hlpr f ts (n-1)

--getPos :: Int -> Int -> Board -> Int 
--getPos y x b = (y * width b) + x
  
--doMove :: String -> Board -> Int -> Int -> Board 
--doMove s b y x = updateBoardAtN f b (getPos y x b) where
--  f = case s of 
--    "m" -> markTile
--    "r" -> revealTile
--    "q" -> queryTile
--    _ -> id  -- id leaves the resultant board unchanged

--updateHelper :: Board -> Board -> Board 
--updateHelper b newb 
--  | newb == b = newb 
--  | otherwise = updateHelper newb (Board newt h w) where
--    h = height newb
--    w = width newb 
--    nextPos x y 
--      | x == w - 1 = (0, y + 1)
--      | otherwise = (x + 1, y)
--    getAdjacent x y offsets = 
    
    
--updateBoard :: Board -> Board
--updateBoard = updateHelper (blankGrid 0 0)

runGame :: Board -> IO ()
runGame b =
  print b -- Debugging game 
  {-
  case getState b of
  Lose -> do 
    print b 
    print "You lose! This is what happens with Donald Trump as President.."
  Win -> do 
    print b
    print "You win! Congratulations!"
  Continue -> do 
    print b 
    print "ENTER: Next move - Mark/Unmark (m) or Reveal (r)?"
    mov <- getLine
    print "ENTER: Row?"
    y <- getLine 
    print "ENTER: Column?"
    x <- getLine 
    let newb = updateBoard $ doMove mov b (read y) (read x)
    rungame newb
  -}

---------------------------------------------
-- Implementation code to initialize game. --
---------------------------------------------
start :: IO ()
start = do
  putStrLn "Recommendations:"
  putStrLn "- Beginner: H(8), W(8), M(10)"
  putStrLn "- Intermediate: H(16), W(16), M(40)"
  putStrLn "- Expert: H(16), W(30), M(99)\n"
  height <- getHeight
  width <- getWidth
  mines <- getMines height width
  runGame (buildGrid (read width) (read height) (read mines))

getHeight :: IO String
getHeight = do
  putStrLn "ENTER: Height? (30 >= H > 0)"
  height <- getLine
  if (isValidNumber height && (read height) <= 30 && (read height) > 0) then
    return height
  else
    do putStrLn "ERROR: Please enter a valid number!"
       getHeight

getWidth :: IO String
getWidth = do
  putStrLn "ENTER: Width? (30 >= W > 0)"
  width <- getLine
  if (isValidNumber width && (read width) <= 30 && (read width) > 0) then
    return width
  else
    do putStrLn "ERROR: Please enter a valid number!"
       getWidth

getMines :: String -> String -> IO String
getMines h w = do
  putStrLn "ENTER: Mines? (H*W > M > 0)"
  mines <- getLine
  if (isValidNumber mines && (read mines) < (((read :: String -> Int) h)*((read :: String -> Int) w)) && (read mines) > 0) then
    return mines
  else
    do putStrLn "ERROR: Please enter a valid number!"
       getMines h w

isValidNumber :: String -> Bool
isValidNumber number = case reads number :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

----------
-- Menu --
----------
menu :: IO ()
menu = do
  putStrLn "Welcome to Minesweeper2!"
  putStrLn "Created by Adam Magdurulan, John Park, & Theodore Lau\n"
  putStrLn "ENTER: start"
  putStrLn "ENTER: quit"




