module Sudoku (Cell, Coordinates, Sudoku (), exampleSudoku, gridSize, solve, subGridSize, sudoku) where

import Data.Array (Array, array, bounds, (!), (//))
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)

type Coordinates = (Natural, Natural)

type Cell = (Coordinates, Natural)

newtype Sudoku = Sudoku (Array Coordinates Natural) deriving (Eq)

instance Show Sudoku where
  show sudoku@(Sudoku arr) = concatMap (\row -> "\n" ++ showRow row) indices
    where
      indices = [1 .. gridSize sudoku]
      showCell row col = let v = arr ! (row, col) in if v == 0 then "." else show v
      showRow row = concatMap (\col -> " " ++ showCell row col) indices

sudoku :: Natural -> [Cell] -> Sudoku
sudoku subGridSize nonEmptyCells = let s = Sudoku (array indexBounds grid) in if isValid s then s else error ("invalid sudoku:" ++ show s)
  where
    gridSize = subGridSize * subGridSize
    indexBounds = ((1, 1), (gridSize, gridSize))
    nonEmptyCoordinates = map fst nonEmptyCells
    emptyCells = [((row, col), 0) | row <- [1 .. gridSize], col <- [1 .. gridSize], (row, col) `notElem` nonEmptyCoordinates]
    grid = nonEmptyCells ++ emptyCells

gridSize, subGridSize :: Sudoku -> Natural
gridSize (Sudoku arr) = fst (snd (bounds arr))
subGridSize sudoku = round (sqrt (fromIntegral (gridSize sudoku)))

firstEmptyCoordinates :: Sudoku -> Maybe Coordinates
firstEmptyCoordinates sudoku@(Sudoku arr) = firstEmpty 1 1
  where
    size = gridSize sudoku
    firstEmpty row col
      | row > size = Nothing
      | col > size = firstEmpty (row + 1) 1
      | otherwise = let cords = (row, col) in if (arr ! cords) == 0 then Just cords else firstEmpty row (col + 1)

isValid :: Sudoku -> Bool
isValid sudoku@(Sudoku arr) = all noDupsInRow rows && all noDupsInCol cols && all noDupsInSub subs
  where
    size = gridSize sudoku
    subSize = subGridSize sudoku
    rows = [1 .. size]
    cols = rows
    subs = [0 .. (size - 1)]
    noDupsInRow row = all (noDupsRow row) cols
    noDupsRow row col = all (\c -> let v = arr ! (row, c) in v == 0 || v /= (arr ! (row, col))) [c | c <- cols, c /= col]
    noDupsInCol col = all (noDupsCol col) rows
    noDupsCol row col = all (\r -> let v = arr ! (r, col) in v == 0 || v /= (arr ! (row, col))) [r | r <- rows, r /= row]
    noDupsInSub sub = let cords = subCords sub in all (noDupsSub cords) cords
    subCords sub =
      let minR = (sub `div` subSize) * subSize + 1
          minC = (sub `mod` subSize) * subSize + 1
       in [(row, col) | row <- [minR .. minR + subSize - 1], col <- [minC .. minC + subSize - 1]]
    noDupsSub cords co = let v = arr ! co in v == 0 || v `notElem` map (arr !) (filter (co /=) cords)

nextConfigs :: Sudoku -> [Sudoku]
nextConfigs sudoku@(Sudoku arr) = maybe [] configs (firstEmptyCoordinates sudoku)
  where
    change cords n = Sudoku (arr // [(cords, n)])
    configs cords = filter isValid [change cords n | n <- [1 .. gridSize sudoku]]

solve :: Sudoku -> [Sudoku]
solve sudoku = do
  next <- nextConfigs sudoku
  if isNothing (firstEmptyCoordinates next) then return next else solve next

-- https://sudoku.tagesspiegel.de/files/2021/14062021_sudoku_hard.pdf
-- https://sudoku.tagesspiegel.de/files/2021/14062021_solve_hard.pdf
exampleSudoku :: Sudoku
exampleSudoku =
  sudoku
    3
    [ ((1, 3), 2),
      ((1, 6), 7),
      ((1, 8), 9),
      ((2, 7), 6),
      ((3, 3), 7),
      ((3, 5), 9),
      ((3, 6), 3),
      ((4, 1), 6),
      ((4, 2), 8),
      ((4, 6), 2),
      ((4, 8), 7),
      ((5, 5), 3),
      ((5, 6), 5),
      ((6, 2), 3),
      ((6, 6), 9),
      ((6, 7), 4),
      ((6, 9), 8),
      ((7, 2), 7),
      ((7, 4), 1),
      ((8, 1), 9),
      ((8, 2), 4),
      ((8, 5), 2),
      ((8, 6), 6),
      ((9, 1), 2),
      ((9, 7), 3)
    ]
