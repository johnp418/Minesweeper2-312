
{- Minesweeper V2 
  Developed by John Park, Theodore Lau, Adam Magdurulan.
  In conjunction with UBC CPSC 312 course curriculum.
  Project #2 of the UBC CPSC 312 Winter 2016T1.
-}

-- TO DEVELOPERS: This project is accompanied by a README, please refer to it for 
--          outstanding issues/changes/commants during the course of development.

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
} deriving (Eq)


data Tile = Tile {
  value :: Int,
  hidden :: Bool,
  marked :: Bool,
  question :: Bool
} deriving (Eq)


-- This needs to be changed!
instance Show Board where
  show b = showHeader (width b) ++ showTiles (tiles b) 0 where
    align3 n
      | n < 10   = ' ' : show n ++ " "
      | n >= 100 = error "Number is too wide to fit here..."
      | n >= 10  = ' ' : show n
    showHeader n = "    " ++ foldl (\a b -> a ++ align3 b) "" [0..n-1] ++ "\n    " ++ foldl (\a b -> a ++ " - ") "" [0..n-1] ++ "\n"
    
    showTiles [] _ = ""
    showTiles ts n = foldl (++) (align3 n ++ "|") (map show thisRow) ++ "\n" ++ showTiles otherRows (n+1) where
      (thisRow, otherRows) = splitAt (width b) ts

instance Show Tile where
  show Tile { question = True } = " ? " -- Question
  show Tile { marked = True } = " ! " -- Marked
  show Tile { hidden = True } = " + " -- Not opened
  show Tile { value = -1 } = " X " -- Mine
  show Tile { value = 0 } = "   " -- Empty
  show Tile { value = x } = " " ++ show x ++ " " -- Mines nearby

newTile :: Tile
newTile = Tile { value = 0, hidden = True, marked = False, question = False }
newMine :: Tile
newMine = Tile { value = -1, hidden = True, marked = False, question = False } -- Mine 

isMine :: Tile -> Bool
isMine t = (value t) == -1

isEmpty :: Tile -> Bool
isEmpty t = (value t) == 0


-- addTile :: Tile -> Int -> Tile
-- addTile (Tile x n) = Tile ((value t) + n)  

--------------------------------------------
-- Implementation code to build the grid. --
--------------------------------------------
buildGrid :: Int -> Int -> Int -> Board
buildGrid w h m = (blankGrid w h) -- mineGrid ((blankGrid w h) m)

blankGrid :: Int -> Int -> Board
blankGrid w h = Board { tiles = [newTile | i <- [1..w], j <- [1..h]], width = w, height = h }

-- mineGrid :: Board -> Int -> Board

--------------------------
-- JOHN
--------------------------

-- setTileValue :: Board -> Board
-- setTileValue b =
--   let
--     h = (height b)
--     w = (width b)

--     next x y =
--       | x == 
    
--     updatedTiles = 1
--   in Board updateTiles h w



--------------------------------------
-- Implementation code to run game. --
--------------------------------------

markTile :: Tile -> Tile 
markTile (Tile v m h _) = Tile v (not m) h False

revealTile :: Tile -> Tile
revealTile (Tile v m _ q) = Tile v m False q

-- Query tile changed marked attr to false, !question
queryTile :: Tile -> Tile 
queryTile (Tile v _ h q) = Tile v False h (not q)

-- Updates the board, applying given function to every tiles in the board
updateBoardAtN :: (Tile -> Tile) -> Board -> Int -> Board 
updateBoardAtN f (Board t h w) n = Board (hlpr f t n) h w where 
	hlpr _ [] _ = []
	hlpr f (t:ts) 0 = f t:ts 
	hlpr f (t:ts) n = t:hlpr f ts (n-1)

--Takes x,y, and board, returns position in Tiles arr
getPos :: Int -> Int -> Board -> Int 
getPos x y b = (y * width b) + x

doMove :: String -> Board -> Int -> Int -> Board 
doMove s b y x = updateBoardAtN f b (getPos x y b) where
  f = case s of
    "m" -> markTile
    "r" -> revealTile
    "q" -> queryTile
    _ -> id  -- id leaves the resultant board unchanged



neighborTiles :: [(Int, Int)]
neighborTiles = [(x,y) | x <- [-1..1] , y <- [-1..1]] -- Pair of (-1,-1), (-1,0), (-1,1), ...,(1,0), (1,1)

boardGet2 :: Board -> Int -> Int -> Tile
boardGet2 b y x
  | y >= 0 && x >= 0 && y < (height b) && x < (width b) = (tiles b) !! (getPos x y b)

updateHelper :: Board -> Board -> Board 
updateHelper b newBoard
  | b == newBoard = b
  | otherwise = updateHelper b (Board newTiles h w) where
      h = (height b)
      w = (width b)
      nextTile x y
        | x == w - 1 = (0, y + 1)
        | otherwise = (x + 1, y)
      getNeighbor x y deltas = boardGet2 b (y + (fst deltas)) (x + (snd deltas))
      
      newTiles = (updateTile (tiles b) 0 0) where
        updateTile [] _ _ = []
        updateTile (Tile n marked hidden question : rest) x y
          | any isEmpty (map (getNeighbor x y) neighborTiles) = Tile n marked False question : updateTile rest x' y'
          where (x', y') = nextTile x y 
        updateTile (t:rest) x y = t : updateTile rest x' y'
          where (x', y') = nextTile x y
      
	
updateBoard :: Board -> Board
updateBoard = updateHelper (blankGrid 0 0)

runGame :: Board -> IO ()
runGame b =
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
      let newb = updateBoard (doMove mov b (read y) (read x))
      runGame newb

data GameState = Lose | Win | Continue | Undetermined
  deriving(Eq, Show)

getTileStatus :: Tile -> GameState
getTileStatus Tile { value = -1, hidden = False, marked = False, question = False } = Lose
getTileStatus Tile { value = 0, hidden = True, marked = False, question = False } = Continue
getTileStatus Tile { value = x, marked = True, question = False } = Continue
getTileStatus _ = Undetermined

getState :: Board -> GameState
getState b = update (tiles b) where
  
  update ts =
    case foldl f Undetermined (map getTileStatus ts) of
      Undetermined -> Win -- GameState must be undetermined up to the last tile
      state -> state -- Otherwise, pass state to next
    where
      f _ Lose = Lose 
      f Lose _ = Lose
      f st Undetermined  = st
      f Continue Continue = Continue
      f st st1 = f st1 st -- Swaps the GameState


                
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

--Takes a string 
getMines :: String -> String -> IO String
getMines h w = do
  putStrLn "ENTER: Mines? (H*W > M > 0)"
  mines <- getLine
  if (isValidNumber mines && (read mines)
      <
      (((read :: String -> Int) h)*((read :: String -> Int) w))
      && (read mines) > 0) then
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
  cmd <- getLine
  if ((read cmd) == "start") then start else menu




