{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers, OverloadedStrings #-}

{- build for CodeWorld environment  -}


import CodeWorld
import Data.Text 

main :: IO()

data Dir = U | D | L | R

type Coords = (Int, Int) 

data Tile = COIN | EMPTY | WALL

runnerPic :: Picture
runnerPic = lettering "\x1F6B6"

ghostPic :: Picture
ghostPic = lettering "\x1F47B"

gameOverPic::Picture
gameOverPic = translated (0) (0) $ colored red $ lettering ("GAME OVER")

gameWinPic::Picture
gameWinPic = translated (0) (0) $ colored green $ lettering ("Congratulations!")

type CollectedCoins = [Coords]

type TileFromCoords = Coords -> Tile

data Level = EASY | MEDIUM | HARD deriving(Eq)

data World = World {
grid :: TileFromCoords,
lvl :: Level,
runnerCoords :: Coords,
ghostCoords :: Coords,
gotCoins :: [Coords],
allCoins :: Int
}

getNextCoords :: Dir -> Coords -> Coords
getNextCoords U (x, y) = (x, y + 1)
getNextCoords D (x, y) = (x, y - 1)
getNextCoords L (x, y) = (x - 1, y)
getNextCoords R (x, y) = (x + 1, y)

canMove :: TileFromCoords -> Coords -> Bool
canMove grid coords =
  case grid coords of
    WALL -> False
    _    -> True

isCoin :: Tile -> Bool
isCoin COIN = True
isCoin _ = False


easyMap :: Coords -> Tile
easyMap coords@(x, y)
  | not (inBounds x y) = EMPTY
  | coords `elem` easyWalls = WALL
  | otherwise = COIN 
  where
    inBounds x y = x >= -8 && x <= 8 && y >= -8 && y <= 8
    easyWalls = [(x, y) | x <- [-8..8], y == -8] ++  
                 [(x, y) | x <- [-8..8], y == 8] ++   
                 [(x, y) | x == -8, y <- [-8..8]] ++ 
                 [(x, y) | x == 8, y <- [-8..8]]    

mediumMap :: Coords -> Tile
mediumMap coords@(x, y)
  | not (inBounds x y) = EMPTY 
  | coords `elem` mazeWalls = WALL
  | otherwise = COIN 
  where
    inBounds x y = x >= -8 && x <= 8 && y >= -8 && y <= 8
    mazeWalls = [(x, y) | x <- [-8..8], y == -8] ++  
                [(x, y) | x <- [-8..8], y == 8] ++  
                [(x, y) | x == -8, y <- [-8..8]] ++ 
                [(x, y) | x == 8, y <- [-8..8]] ++  
                [(x, y) | x == 2, y <- [-5..0]] ++
                [(x, y) | x <- [2..8], y == 2] ++
                [(x, y) | x <- [-3..3], y == 4 ] ++
                [(x, y) | x <- [0..8], y == -2 ] ++
                [(x, y) | x <- [-5..3], y == -5] ++
                [(x, y) | x <- [0.. 6], y == -7] ++ 
                [(x, y) | x <- [2..9], y == 6] ++
                [(x, y) | y <- [2..9], x == -3] ++
                [(x, y) | y <- [-6..0], x == -6] ++
                [(x, y) | y <- [-8..(-7)], x == -4] ++ 
                [(x, y) | y <- [-6..(0)], x == -2] ++
                [(x, y) | x <- [-8..(-6)], y == 5] ++
                [(x, y) | y <- [7..8], x == -6] ++
                [(x, y) | y <- [5..(-5)], x == -4] ++
                [(x, y) | x <- [(-6)..(-4)], y == 3]
                
hardMap :: Coords -> Tile
hardMap coords@(x, y)
  | not (inBounds x y) = EMPTY
  | coords `elem` mazeWalls = WALL
  | otherwise = COIN
  where
    inBounds x y = x >= -8 && x <= 8 && y >= -8 && y <= 8

    -- Helper function to create horizontal wall segments
    horizontalWall y xs = [(x, y) | x <- xs]
    -- Helper function to create vertical wall segments
    verticalWall x ys = [(x, y) | y <- ys]

    -- List comprehension for outer walls
    outerWalls = [(x, y) | x <- [-8..8], elem y [-8, 8]] ++
                 [(x, y) | y <- [-7..7], elem x [-8, 8]]

    -- Additional inner walls
    innerWalls = Prelude.concat [
      horizontalWall (-2) [0..8],
      horizontalWall 2 [-8..(-5)],
      horizontalWall 4 [0..3],
      horizontalWall (-5) [-5..3],
      horizontalWall (-7) [0..6],
      horizontalWall 6 [2..9],
      verticalWall 2 [-5..0],
      verticalWall (-3) [2..9],
      verticalWall (-6) [-6..0],
      verticalWall (-4) [(-3)..0],
      verticalWall (-6) [7..8]]

    -- Define mazeWalls by combining the outer walls and the inner walls
    mazeWalls = outerWalls ++ innerWalls ++ 
                -- Add more wall coordinates here to increase complexity
                horizontalWall (-1) [0..7] ++  -- Example additional wall
                verticalWall 4 [0..3]         -- Example additional wall
      

                
easyWorld :: World
easyWorld = World{
grid = easyMap,
lvl = EASY,
runnerCoords = (0, 0),
ghostCoords = (1, 0),
gotCoins = [],
allCoins = 10
}

mediumWorld :: World
mediumWorld = World{
grid = mediumMap,
lvl = MEDIUM,
runnerCoords = (0, 0),
ghostCoords = (1, 0),
gotCoins = [],
allCoins = 10
}

hardWorld :: World
hardWorld = World{
grid = hardMap,
lvl = HARD,
runnerCoords = (0, 0),
ghostCoords = (1, 0),
gotCoins = [],
allCoins = 10
}



upgradeWorld :: Level -> World
upgradeWorld EASY = easyWorld
upgradeWorld MEDIUM = mediumWorld
upgradeWorld HARD = hardWorld




tryMoveRunner :: World -> Dir -> World
tryMoveRunner (World grid lvl runnerCoords ghostCoords gotCoins allCoins) dir
  | Prelude.length gotCoins == allCoins && lvl == EASY = upgradeWorld MEDIUM
  | Prelude.length gotCoins == allCoins && lvl == MEDIUM = upgradeWorld HARD
  | canMove grid newRunnerCoords = World grid lvl newRunnerCoords ghostCoords newCoins allCoins
  | otherwise = World grid lvl runnerCoords ghostCoords gotCoins allCoins
  where
    (x, y) = runnerCoords
    (a, b) = ghostCoords
  
    newRunnerCoords = if (x == a) && (y == b) then runnerCoords else getNextCoords dir runnerCoords
    newCoins = if isCoin (grid newRunnerCoords) && not (newRunnerCoords `elem` gotCoins) then (gotCoins ++ [newRunnerCoords]) else gotCoins

tryMoveGhost :: World -> Dir -> World
tryMoveGhost (World grid lvl runnerCoords ghostCoords gotCoins allCoins) dir
  | canMove grid newGhostCoords = World grid lvl runnerCoords newGhostCoords gotCoins allCoins
  | otherwise = World grid lvl runnerCoords ghostCoords gotCoins allCoins
  where
    (x, y) = runnerCoords
    (a, b) = ghostCoords
  
    newGhostCoords = if (x == a) && (y == b) then ghostCoords else getNextCoords dir ghostCoords

tryMove :: Int -> World -> Dir -> World
tryMove n world dir
  | n == 0 = tryMoveRunner world dir
  | otherwise = tryMoveGhost world dir


drawTile :: Tile -> Picture
drawTile COIN = colored yellow (solidCircle 0.2)
drawTile WALL = colored black (solidRectangle 1 1)
drawTile _ = blank

drawTiles :: [Tile] -> [Coords] -> Picture
drawTiles [] [] = blank
drawTiles (t : tiles) ((x, y) : coordsRest) = 
  translated (fromIntegral x) (fromIntegral y) (drawTile t) <> 
  (drawTiles tiles coordsRest)

coordsGenerator :: Int -> [Coords]
coordsGenerator size = [(x, y) | x <- [-size..size], y <- [-size..size]]

printScore :: [a] -> Picture
printScore gotCoins = translated (-5) (-9) $ colored green $ lettering (pack ("Score: " ++ show (Prelude.length gotCoins)))


getCoin :: Tile -> Coords -> CollectedCoins -> Tile
getCoin tile coords coins
  | isCoin tile && coords `elem` coins = EMPTY
  | otherwise = tile

getTile :: World -> Coords -> Tile 
getTile world@(World grid lvl (x, y) (xx, yy) gotCoins allCoins) curCoords = getCoin tile curCoords gotCoins
    where tile = grid  curCoords

drawWorld :: Int -> World -> Picture
drawWorld _ world@(World grid lvl (x, y) (a, b) gotCoins allCoins)
  | (x == a) && (y == b) = gameOverPic
  | Prelude.length gotCoins >= allCoins && lvl == HARD = gameWinPic 
  | otherwise = 
      translated (fromIntegral x) (fromIntegral y) runnerPic <>
      translated (fromIntegral a) (fromIntegral b) ghostPic <>
      drawTiles tileList coordsList <> scorePic
    where 
      scorePic = printScore gotCoins
      tileList = Prelude.map (getTile world) coordsList
      coordsList = coordsGenerator 12
 
 
handleEvent :: Int -> Event -> World -> World
handleEvent pID e world
  | e == (KeyPress "Up") = tryMove pID world U
  | e == (KeyPress "Down") = tryMove pID world D
  | e == (KeyPress "Left") = tryMove pID world L
  | e == (KeyPress "Right") = tryMove pID world R
  | otherwise = world
  
  
main = groupActivityOf 1 init step view
  where 
    init = static (\gen -> easyWorld)
    step = static (\playerNumber event world -> handleEvent playerNumber event world)
    view = static (\playerNumber world -> drawWorld playerNumber world)




