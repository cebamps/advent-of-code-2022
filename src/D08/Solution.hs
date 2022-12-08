module D08.Solution where

import AOC.Parser
import Control.Comonad.Env (Comonad (extract))
import Control.Comonad.Store.Pointer (Pointer, experiment, pointer, pointerBounds)
import Data.Array (listArray)
import Text.Megaparsec

type Dim = (Int, Int)

type Idx = (Int, Int)

data Direction = East | West | North | South deriving (Eq, Show)

type Grid = Pointer Idx

-- basic utilities to find our bearings

idxPlus :: Idx -> Idx -> Idx
idxPlus (x, y) (dx, dy) = (x + dx, y + dy)

unit :: Direction -> Idx
unit North = (0, -1)
unit South = (0, 1)
unit West = (-1, 0)
unit East = (1, 0)

idxClamp :: Dim -> Idx -> Idx
idxClamp (mx, my) (x, y) = (clamp (0, mx - 1) x, clamp (0, my - 1) y)
  where
    clamp :: Ord a => (a, a) -> a -> a
    clamp (l, r) = max l . min r

idxWithin :: Idx -> Idx -> [Idx]
idxWithin (x1, y1) (x2, y2) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

-- comonadic grid fun

-- TODO: working on boundaries rather than dimensions would be nicer
gridDims :: Grid a -> Dim
gridDims g = let ((0, 0), (hx, hy)) = pointerBounds g in (hx + 1, hy + 1)

toEdge :: Dim -> Direction -> Idx -> [Idx]
toEdge dim dir pos = idxWithin ((pos `idxPlus` unit dir) `idxClamp` dim) (extreme dim dir pos)
  where
    extreme _ North (x, _) = (x, 0)
    extreme _ West (_, y) = (0, y)
    extreme (mx, _) East (_, y) = (mx - 1, y)
    extreme (_, my) South (x, _) = (x, my - 1)

lookToEdge :: Direction -> Grid a -> [a]
lookToEdge dir = do
  -- reader monad on (Grid a)
  dim <- gridDims
  experiment (toEdge dim dir)

-- our actual tree business

visibleFrom :: Ord a => Direction -> Grid a -> Bool
visibleFrom dir = do
  focused <- extract
  others <- lookToEdge dir
  return $ all (focused >) others

visible :: Ord a => Grid a -> Bool
visible g = or [visibleFrom dir g | dir <- [North, East, West, South]]

-- and now we implement the grid!
grid :: Dim -> [a] -> Maybe (Grid a)
grid (mx, my) items
  | length items == mx * my =
    let arr = listArray ((0, 0), (mx - 1, my - 1)) items
     in Just . pointer arr $ (0, 0)
  | otherwise = Nothing

-- experiment (toEdge d North)

type Input = ()

---

solve1 :: Input -> IO ()
solve1 inp = return ()

solve2 :: Input -> IO ()
solve2 inp = return ()

--- $> readFile "inputs/d08-test.txt" >>= solve

---

inputP :: Parser Input
inputP = undefined <* eof

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
