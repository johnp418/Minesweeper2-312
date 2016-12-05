
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
  value :: TileVal,
  marked :: Bool,
  hidden :: Bool,
  question :: Bool
} deriving (Eq)

data TileVal = Value Int | Mine deriving (Eq)

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

instance Show Tile where
  show Tile { question = True } = " ? " -- Question
  show Tile { marked = True } = " ! " -- Marked
  show Tile { hidden = True } = " _ " -- Not opened
  show Tile { value = Mine } = " X " -- Mine
  show Tile { value = (Value x) } = " " ++ show x ++ " " -- Mines nearby

newTile :: Tile
newTile = Tile { value = Value 0, hidden = True, marked = False, question = False }
newMine :: Tile
newMine = Tile { value = Mine , hidden = True, marked = False, question = False } -- Mine 

isMine :: Tile -> Bool
isMine t = (value t) == Mine

isEmpty :: Tile -> Bool
isEmpty t = ((value t) == Value 0) && not (hidden t)

nextTilePos :: Int -> Int -> Board -> (Int, Int)
nextTilePos x y b
  | x == (width b) - 1 = (0, y+1)
  | otherwise = (x+1, y)

--------------------------------------------
-- Implementation code to build the grid. --
--------------------------------------------
buildGrid :: Int -> Int -> Int -> StdGen -> Board
buildGrid w h m rng = setTilesValue (mineGrid (blankGrid w h) m rng)

blankGrid :: Int -> Int -> Board
blankGrid w h = Board { tiles = [newTile | i <- [1..w], j <- [1..h]], width = w, height = h }

totalNumTiles :: Board -> Int
totalNumTiles b = (width b) * (height b)

countNearbyMines :: [Tile] -> Int
countNearbyMines tiles = length (filter isMine tiles)

addMineCountToTile :: Tile -> Int -> Tile
addMineCountToTile (Tile Mine m h q) n = Tile Mine m h q
addMineCountToTile (Tile (Value x) m h q) n = Tile (Value (x+n)) m h q

--Takes x,y, and board, returns position in Tiles arr
getPos :: Int -> Int -> Board -> Int 
getPos x y b = (y * width b) + x

-- 3x3 Neighbor tiles around the center
neighborTiles :: [(Int, Int)]
neighborTiles = [(x,y) | x <- [-1..1] , y <- [-1..1]] -- Pair of (-1,-1), (-1,0), (-1,1), ...,(1,0), (1,1)

getDiffPair :: Int -> Int -> (Int,Int) -> (Int,Int)
getDiffPair x y (x1,y1) = (x + x1, y + y1)

getTilesInRange :: Board -> [(Int, Int)] -> [Tile]
getTilesInRange b pairList = lst where
  h = height b
  w = width b
  test (x,y) = (x >= 0 && y >= 0 && x < w && y < h)  
  toTiles = filter test pairList
  toTile (x,y) = (tiles b) !! (getPos x y b)
  lst = map toTile toTiles

setTilesValue :: Board -> Board
setTilesValue b = let
  w = (width b)
  h = (height b)

  tilesNearby x y b = getTilesInRange b (map (getDiffPair x y) neighborTiles)

  replaceTiles [] _ = []
  replaceTiles (t:rest) (x,y) = addMineCountToTile t (countNearbyMines (tilesNearby x y b)) :
    replaceTiles rest (nextTilePos x y b)
    
  replacedTiles = replaceTiles (tiles b) (0,0)

  in Board replacedTiles h w
  
-- Takes a board, Replaces tiles with mines
mineGrid :: Board -> Int -> StdGen -> Board
mineGrid b 0 _ = b
mineGrid b n rng = newBoard where
  newBoard = Board { tiles = replacedTiles, height = (height b), width = (width b) } where
    plantMine ts rng 0 = ts
    plantMine (tile:rest) rng n
      | rngValue <= fromIntegral n / fromIntegral (length (tile:rest)) = newMine : plantMine rest rng' (n - 1)
      | otherwise = tile : plantMine rest rng' n
      where
        (rngValue, rng') = randomR(0,1)
         rng :: (Double, StdGen)
    
    replacedTiles = plantMine (tiles b) rng n

-- Mark tile 
markTile :: Tile -> Tile 
markTile (Tile v m h _) = Tile v (not m) h False

-- Open tile
revealTile :: Tile -> Tile
revealTile (Tile v m _ q) = Tile v m False q

-- Question mark to tile
questionTile :: Tile -> Tile 
questionTile (Tile v _ h q) = Tile v False h (not q)

-- Updates the board, applying given function to nth tile in the board
updateBoardAtN :: (Tile -> Tile) -> Board -> Int -> Board 
updateBoardAtN f (Board t h w) n = Board (hlpr f t n) h w where  
  hlpr _ [] _ = []
  hlpr f (t:ts) 0 = f t:ts 
  hlpr f (t:ts) n = t:hlpr f ts (n-1)

doMove :: String -> Board -> Int -> Int -> Board 
doMove s b y x = updateBoardAtN f b (getPos x y b) where
  f = case s of
    "m" -> markTile
    "r" -> revealTile
    "q" -> questionTile
    _ -> id  -- id leaves the resultant board unchanged


updateHelper :: Board -> Board -> Board 
updateHelper b newBoard
  | b == newBoard = b
  | otherwise = updateHelper newBoard (Board newTiles h w) where
      h = (height newBoard)
      w = (width newBoard)

      tilesNearby x y board = getTilesInRange board (map (getDiffPair x y) neighborTiles)

      updateTile [] _ = []
      updateTile (Tile (Value n) marked hidden question : rest) (x,y)
        | any isEmpty (tilesNearby x y newBoard) = Tile (Value n) marked False question : updateTile rest (nextTilePos x y newBoard)

      updateTile (t:rest) (x,y) = t : updateTile rest (nextTilePos x y newBoard)
      
      newTiles = updateTile (tiles newBoard) (0,0)

updateBoard :: Board -> Board
updateBoard = updateHelper (blankGrid 0 0)

getInputY :: Board -> IO String
getInputY b = do
  print "ENTER: Row?"
  y <- getLine
  if (isValidNumber y && (read y) <= (height b) && (read y) > 0) then
    return y
  else
    do putStrLn "ERROR: Please enter a valid row number!"
       getInputY b
	 
getInputX :: Board -> IO String
getInputX b = do
  print "ENTER: Column?"
  x <- getLine
  if (isValidNumber x && (read x) <= (width b) && (read x) > 0) then
    return x
  else
    do putStrLn "ERROR: Please enter a valid column number!"
       getInputX b
	   
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
      print "ENTER: Next move - Mark/Unmark (m) / Reveal (r) / Question Mark (q) ?"
      mov <- getLine
      y <- (getInputY b)
      x <- (getInputX b)
      let newb =  updateBoard (doMove mov b ((read y) - 1) ((read x)-1))
      runGame newb

      -- let newb = updateBoard (doMove mov b (read y) (read x)) <- Not working atm

data GameState = Lose | Win | Continue | Undetermined
  deriving(Eq, Show)

getTileStatus :: Tile -> GameState
getTileStatus Tile { value = Mine, hidden = False, marked = False, question = False } = Lose
getTileStatus Tile { value = Mine, hidden = True, marked = False, question = False } = Continue
getTileStatus Tile { value = (Value n), hidden = False, marked = True, question = False } = Continue
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
      f state Win = state
      f state Undetermined  = state
      f Continue Continue = Continue
      f state state1 = f state1 state -- Swaps the GameState


                
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
  rng <- newStdGen
  runGame (buildGrid (read width) (read height) (read mines) rng)

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
  putStrLn "ENTER: :quit"




