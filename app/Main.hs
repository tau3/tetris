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
  | J
  | S
  | T
  | Z
  | I
  deriving (Show, Eq)

data Figure = Figure
  { template :: Template
  , leftTop :: Point
  , state :: Int
  , color' :: Color
  } deriving (Show)

data Game = Game
  { grid :: Grid
  , figure :: Figure
  , stdGen :: StdGen
  } deriving (Show)

-- rotations are stolen from https://github.com/brenns10/tetris/
rotations :: [(Template, [[Point]])]
rotations =
  [ ( I
    , [ [(1, 0), (1, 1), (1, 2), (1, 3)]
      , [(0, 2), (1, 2), (2, 2), (3, 2)]
      , [(3, 0), (3, 1), (3, 2), (3, 3)]
      , [(0, 1), (1, 1), (2, 1), (3, 1)]
      ])
  , ( J
    , [ [(0, 0), (1, 0), (1, 1), (1, 2)]
      , [(0, 1), (0, 2), (1, 1), (2, 1)]
      , [(1, 0), (1, 1), (1, 2), (2, 2)]
      , [(0, 1), (1, 1), (2, 0), (2, 1)]
      ])
  , ( L
    , [ [(0, 2), (1, 0), (1, 1), (1, 2)]
      , [(0, 1), (1, 1), (2, 1), (2, 2)]
      , [(1, 0), (1, 1), (1, 2), (2, 0)]
      , [(0, 0), (0, 1), (1, 1), (2, 1)]
      ])
  , ( O
    , [ [(0, 1), (0, 2), (1, 1), (1, 2)]
      , [(0, 1), (0, 2), (1, 1), (1, 2)]
      , [(0, 1), (0, 2), (1, 1), (1, 2)]
      , [(0, 1), (0, 2), (1, 1), (1, 2)]
      ])
  , ( S
    , [ [(0, 1), (0, 2), (1, 0), (1, 1)]
      , [(0, 1), (1, 1), (1, 2), (2, 2)]
      , [(1, 1), (1, 2), (2, 0), (2, 1)]
      , [(0, 0), (1, 0), (1, 1), (2, 1)]
      ])
  , ( T
    , [ [(0, 1), (1, 0), (1, 1), (1, 2)]
      , [(0, 1), (1, 1), (1, 2), (2, 1)]
      , [(1, 0), (1, 1), (1, 2), (2, 1)]
      , [(0, 1), (1, 0), (1, 1), (2, 1)]
      ])
  , ( Z
    , [ [(0, 0), (0, 1), (1, 1), (1, 2)]
      , [(0, 2), (1, 1), (1, 2), (2, 1)]
      , [(1, 0), (1, 1), (2, 1), (2, 2)]
      , [(0, 1), (1, 0), (1, 1), (2, 0)]
      ])
  ]

initialState :: Game
initialState = Game grid f stdGen'
  where
    stdGen = mkStdGen seed
    (f, stdGen') = randomFigure stdGen
    grid =
      [((r, c), background) | r <- [0 .. rows - 1], c <- [0 .. columns - 1]]

colors :: [Color]
colors =
  [yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

columns :: Int
columns = 10

rows :: Int
rows = 20

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
figureToGrid (Figure t (x, y) s c) = map shift (getRotation t s)
  where
    getRotation t' s' =
      snd (head $ filter (\tpl -> fst tpl == t') rotations) !! s'
    shift = (, c) . (\(px, py) -> (px + x, py + y))

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
moveRight (Figure t (r, c) clr s) = Figure t (r, c + 1) clr s

moveLeft :: Figure -> Figure
moveLeft (Figure t (r, c) clr s) = Figure t (r, c - 1) clr s

moveDown :: Figure -> Figure
moveDown (Figure t (r, c) clr s) = Figure t (r + 1, c) clr s

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
        else g
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) g@Game { grid
                                                            , figure
                                                            , stdGen
                                                            } =
  let turned = turn figure
   in if turned `fits` grid
        then Game grid turned stdGen
        else g
handleKeys _ g = g

turn :: Figure -> Figure
turn (Figure t lt s c) = Figure t lt s' c
  where
    s' =
      if s == 3
        then 0
        else s + 1

isHitTheGround :: Game -> Bool
isHitTheGround Game {grid, figure} = not $ moveDown figure `fits` grid

fps :: Int
fps = 60

randomInt :: StdGen -> Int -> (Int, StdGen)
randomInt rand limit = (result, rand')
  where
    (n, rand') = random rand :: (Int, StdGen)
    result = n `mod` limit

randomFigure :: StdGen -> (Figure, StdGen)
randomFigure stdGen = (f, stdGen''')
  where
    (clr, stdGen') = randomInt stdGen (length colors)
    (template, stdGen'') = randomInt stdGen' (length rotations)
    (state, stdGen''') = randomInt stdGen'' 4
    f =
      Figure
        (fst (rotations !! template))
        (0, columns `div` 2)
        state
        (colors !! clr)

updateIfHitTheGround :: Float -> Game -> Game
updateIfHitTheGround _ g@Game {grid, figure, stdGen} =
  if isHitTheGround g
    then Game grid' figure' stdGen'
    else g
  where
    grid' = merge grid (figureToGrid figure)
    (figure', stdGen') = randomFigure stdGen

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

{-# NOINLINE seed #-}
seed :: Int
seed = unsafePerformIO $ round `fmap` getPOSIXTime

removeFullRows :: Float -> Game -> Game
removeFullRows f g@(Game grid figure stdGen) =
  case fullRow of
    Just n -> removeFullRows f (Game (removeFullRow grid n) figure stdGen)
    Nothing -> g
  where
    fullRow = getFullRow grid

checksBeforeHitTheGround :: Float -> Game -> Game
checksBeforeHitTheGround f g@Game {figure, grid} =
  if False
    then let moved = moveDown figure
          in if moved `fits` grid
               then g {grid, figure = moved}
               else g
    else g

update :: Float -> Game -> Game
update f g =
  if isHitTheGround g
    then let g' = updateIfHitTheGround f g
             g'' = removeFullRows f g'
          in g''
    else g

{-
update :: Float -> Game -> Game
update f g =
  let g' = checksBeforeHitTheGround f g
   in if isHitTheGround g'
        then updateIfHitTheGround f g'
        else g'
-}
main :: IO ()
main = play window background fps initialState render handleKeys update
