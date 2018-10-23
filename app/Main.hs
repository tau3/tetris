{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Time.Clock.POSIX
import Debug.Trace
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import System.IO.Unsafe
import System.Random

type Point = (Int, Int)

type Cell = (Point, Color)

type Grid = [Cell]

data Template
  = O
  | L
  deriving (Show, Eq)

data Figure = Figure
  { template :: Template
  , leftTop :: Point
  , color' :: Color
  } deriving (Show)

data Game = Game
  { grid :: Grid
  , figure :: Figure
  , stdGen :: StdGen
  } deriving (Show)

initialState :: Game
initialState = Game grid (Figure (template figure) (leftTop figure) clr) stdGen
  where
    initial@Game {grid, figure} =
      Game
        [((r, c), black) | r <- [0 .. rows - 1], c <- [0 .. columns - 1]]
        (Figure O (0, columns `div` 2) yellow)
        (mkStdGen seed)
    (clr, Game {stdGen}) = randomColor initial

colors :: [Color]
colors =
  [yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

columns :: Int
columns = 4

rows :: Int
rows = 7

cellSize :: Int
cellSize = 20

width :: Int
width = columns * cellSize

height :: Int
height = rows * cellSize

window :: Display
window = InWindow "Tetris" (width, height) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

renderCell :: Cell -> Picture
renderCell ((row, column), clr) =
  translate (fromIntegral xCenter) (fromIntegral yCenter) $
  color clr $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
  where
    xCenter = width `div` 2 * (-1) + cellSize `div` 2 + (column * cellSize)
    yCenter = height `div` 2 - cellSize `div` 2 - (row * cellSize)

figureToGrid :: Figure -> Grid
figureToGrid (Figure t (x, y) c)
  | t == O = map ((, c) . (\(px, py) -> (px + x, py + y))) initO
  | otherwise = error "not implemented yet"
  where
    initO = [(0, 0), (0, 1), (1, 0), (1, 1)]

hasPoint :: Grid -> Point -> Bool
hasPoint g p = p `elem` map fst g

merge :: Grid -> Grid -> Grid
merge acc i = filter (not . hasPoint i . fst) acc ++ i

render :: Game -> Picture
render Game {grid, figure} =
  let fGrid = figureToGrid figure
      resGrid = merge grid fGrid
   in pictures $ map renderCell resGrid

at :: Grid -> Point -> Cell
at cs p = head $ filter (\c -> fst c == p) cs

moveRight :: Figure -> Figure
moveRight (Figure t (r, c) clr) = Figure t (r, c + 1) clr

moveLeft :: Figure -> Figure
moveLeft (Figure t (r, c) clr) = Figure t (r, c - 1) clr

moveDown :: Figure -> Figure
moveDown (Figure t (r, c) clr) = Figure t (r + 1, c) clr

fits :: Figure -> Grid -> Bool
fits f g =
  let asGrid = figureToGrid f
      fit p = g `hasPoint` p && (snd (g `at` p) == black)
      ps = map fst asGrid
   in all fit ps

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g@Game { grid
                                                            , figure
                                                            , stdGen
                                                            } =
  let moved = moveRight figure
   in if moved `fits` grid
        then Game grid moved stdGen
        else g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g@Game { grid
                                                           , figure
                                                           , stdGen
                                                           } =
  let moved = moveLeft figure
   in if moved `fits` grid
        then Game grid moved stdGen
        else g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g@Game { grid
                                                           , figure
                                                           , stdGen
                                                           } =
  let moved = moveDown figure
   in if moved `fits` grid
        then Game grid moved stdGen
        else traceShowId g
handleKeys _ g = g

isHitTheGround :: Game -> Bool
isHitTheGround Game {grid, figure} = not $ moveDown figure `fits` grid

fps :: Int
fps = 60

randomColor :: Game -> (Color, Game)
randomColor Game {grid, figure, stdGen} = (clr, g')
  where
    (i, r) = random stdGen :: (Int, StdGen)
    i' = i `mod` length colors
    clr = colors !! i'
    g' = Game grid figure r

updateIfHitTheGround :: Float -> Game -> Game
updateIfHitTheGround _ g@Game {grid, figure} =
  if isHitTheGround g
    then Game grid' figure' stdGen
    else g
  where
    (clr, Game {stdGen}) = randomColor g
    grid' = merge grid (figureToGrid figure)
    figure' = Figure O (0, columns `div` 2) clr

getRow :: Grid -> Int -> [Cell]
getRow g n = filter (\((r, _), _) -> r == n) g

isFull :: [Cell] -> Bool
isFull = all (\((_, _), clr) -> clr /= black)

removeFullRow :: Grid -> Int -> Grid
removeFullRow g n =
  let g' = filter (\((r, _), _) -> r /= n) g
      g'' =
        map
          (\((r, c), clr) ->
             let r' =
                   if r < n
                     then r + 1
                     else r
              in ((r', c), clr))
          g'
      emptyTopRow = [((0, c), black) | c <- [0 .. columns - 1]]
   in emptyTopRow ++ g''

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

getFullRow :: Grid -> Maybe Int
getFullRow g =
  maybeHead $
  map fst $
  filter snd $
  map (\(i, r) -> (i, isFull r)) $ map (\i -> (i, getRow g i)) [0 .. rows - 1]

seed :: Int
seed = unsafePerformIO $ round `fmap` getPOSIXTime

removeFullRows :: Float -> Game -> Game
removeFullRows f g@(Game grid figure stdGen) =
  case fullRow of
    Just n -> removeFullRows f (Game (removeFullRow grid n) figure stdGen)
    Nothing -> g
  where
    fullRow = getFullRow grid

update :: Float -> Game -> Game
update f g =
  if isHitTheGround g
    then let g' = updateIfHitTheGround f g
             g'' = removeFullRows f g'
          in g''
    else g

main :: IO ()
main = play window background fps initialState render handleKeys update
