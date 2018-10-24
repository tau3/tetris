{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Time.Clock.POSIX
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

background :: Color
background = black

renderCell :: Cell -> Picture
renderCell ((row, column), clr) =
  translate (fromIntegral xCenter) (fromIntegral yCenter) $
  color clr $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
  where
    xCenter = width `div` 2 * (-1) + cellSize `div` 2 + (column * cellSize)
    yCenter = height `div` 2 - cellSize `div` 2 - (row * cellSize)

figureToGrid :: Figure -> Grid
figureToGrid (Figure template (x, y) state clr) = map shift rotation
  where
    rotation =
      snd (head $ filter (\tpl -> fst tpl == template) rotations) !! state
    shift = (, clr) . (\(x', y') -> (x' + x, y' + y))

hasPoint :: Grid -> Point -> Bool
hasPoint g p = p `elem` map fst g

merge :: Grid -> Grid -> Grid
merge acc i = filter (not . hasPoint i . fst) acc ++ i

render :: Game -> Picture
render Game {grid, figure} =
  let fGrid = figureToGrid figure
      resGrid = merge grid fGrid
   in pictures $ map renderCell resGrid

moveDown :: Figure -> Figure
moveDown f@Figure {leftTop = (r, c)} = f {leftTop = (r + 1, c)}

fits :: Figure -> Grid -> Bool
fits figure grid = all fit points
  where
    fit p = grid `hasPoint` p && (snd (grid `at` p) == background)
    points = map fst $ figureToGrid figure
    at grid' p' = head $ filter (\c -> fst c == p') grid'

adjustFigure :: (Figure -> Figure) -> Game -> Game
adjustFigure f g@Game {grid, figure} =
  let figure' = f figure
   in if figure' `fits` grid
        then g {figure = figure'}
        else g

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g =
  adjustFigure moveRight g
  where
    moveRight f@Figure {leftTop = (r, c)} = f {leftTop = (r, c + 1)}
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g = adjustFigure moveLeft g
  where
    moveLeft f@Figure {leftTop = (r, c)} = f {leftTop = (r, c - 1)}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g = adjustFigure moveDown g
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) g = adjustFigure turn g
  where
    turn f@Figure {state} = f {state = (state + 1) `mod` 4}
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) g = dropDownFigure g
handleKeys _ g = g

dropDownFigure :: Game -> Game
dropDownFigure g@Game {figure, grid} =
  let moved = moveDown figure
   in if moved `fits` grid
        then dropDownFigure g {figure = moved}
        else g

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

ifThen :: (a -> Bool) -> (a -> a) -> a -> a
ifThen p f x =
  if p x
    then f x
    else x

removeFullRow :: Grid -> Int -> Grid
removeFullRow g n = emptyTopRow ++ g''
  where
    isOnFullRow ((r, _), _) = r == n
    g'' = map maybeMoveDownCell $ filter (not . isOnFullRow) g
    maybeMoveDownCell ((r, c), clr) = ((r', c), clr)
      where
        r' = ifThen (< n) (+ 1) r
    emptyTopRow = [((0, c), background) | c <- [0 .. columns - 1]]

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

getFullRow :: Grid -> Maybe Int
getFullRow = maybeHead . map fst . filter (isFull . snd) . enumeratedRows
  where
    enumeratedRows g = zip [0 ..] $ map (getRow g) [0 .. rows - 1]
    getRow g n = filter (\((r, _), _) -> r == n) g :: [Cell]
    isFull = all (\c -> snd c /= background) :: [Cell] -> Bool

{-# NOINLINE seed #-}
seed :: Int
seed = unsafePerformIO $ round `fmap` getPOSIXTime

removeFullRows :: Game -> Game
removeFullRows g@Game {grid} =
  case fullRow of
    Just n -> removeFullRows g {grid = removeFullRow grid n}
    Nothing -> g
  where
    fullRow = getFullRow grid

maybeMoveDownFigure :: Game -> Game
maybeMoveDownFigure g@Game {figure, grid} =
  if moved `fits` grid
    then g {figure = moved}
    else g
  where
    moved = moveDown figure

spawnNewFigure :: Game -> Game
spawnNewFigure Game {grid, figure, stdGen} = Game grid' figure' stdGen'
  where
    grid' = merge grid (figureToGrid figure)
    (figure', stdGen') = randomFigure stdGen

update :: Float -> Game -> Game
update _ g@Game {grid, figure} =
  if isHitTheGround
    then removeFullRows $ spawnNewFigure g'
    else g'
  where
    g' = maybeMoveDownFigure g
    isHitTheGround = not $ moveDown figure `fits` grid

main :: IO ()
main = play window background fps initialState render handleKeys update
  where
    window = InWindow "Tetris" (width, height) (10, 10)
    fps = 1
