module D08.Solution where

import AOC.Parser
import Control.Comonad (Comonad (extend))
import Control.Comonad.Env (Comonad (extract))
import Control.Comonad.Store.Pointer (Pointer, experiment, pointer, pointerBounds, runPointer)
import Data.Array (listArray)
import Data.Foldable (toList)
import Data.List (delete)
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char (digitChar, eol)

type Dim = (Int, Int)

type Idx = (Int, Int)

data Direction = East | West | North | South deriving (Eq, Show)

type Grid = Pointer Idx

type Input = Grid Int

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
idxWithin (x1, y1) (x2, y2) = [(x, y) | x <- range x1 x2, y <- range y1 y2]

range :: (Enum a, Ord a) => a -> a -> [a]
range x y
  | x < y = [x .. y]
  | otherwise = [x, pred x .. y]

-- comonadic grid fun

-- TODO: working on boundaries rather than dimensions would be nicer
gridDims :: Grid a -> Dim
gridDims g = let ((0, 0), (hx, hy)) = pointerBounds g in (hx + 1, hy + 1)

toEdge :: Dim -> Direction -> Idx -> [Idx]
toEdge dim dir pos = delete pos $ idxWithin pos (extreme dim dir pos)
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

isVisibleFromEdge :: Ord a => Direction -> Grid a -> Bool
isVisibleFromEdge dir = do
  focused <- extract
  others <- lookToEdge dir
  return $ all (focused >) others

isVisibleFromOutside :: Ord a => Grid a -> Bool
isVisibleFromOutside g = or [isVisibleFromEdge dir g | dir <- [North, East, West, South]]

numVisibleToward :: Direction -> Grid Int -> Int
numVisibleToward dir = do
  focusedHeight <- extract
  heights <- lookToEdge dir
  return $ count focusedHeight heights
  where
    count :: Int -> [Int] -> Int
    count h heights =
      let (vis, block) = span (< h) heights
       in length vis + case block of
            [] -> 0
            _ -> 1

scenicScore :: Grid Int -> Int
scenicScore g = product [numVisibleToward dir g | dir <- [North, East, West, South]]

-- and now we implement the grid!
grid :: Dim -> [a] -> Maybe (Grid a)
grid (mx, my) items
  | length items == mx * my =
    let arr = listArray ((0, 0), (mx - 1, my - 1)) items
     in Just . pointer arr $ (0, 0)
  | otherwise = Nothing

---

solve1 :: Input -> IO ()
solve1 inp =
  let visibleArray = fst . runPointer $ extend isVisibleFromOutside inp
      numVisible = length . filter id . toList $ visibleArray
   in print numVisible

solve2 :: Input -> IO ()
solve2 inp =
  let scenicScores = fst . runPointer $ extend scenicScore inp
   in print $ maximum scenicScores

---

inputP :: Parser Input
inputP = do
  rows <- some (read . (: []) <$> digitChar) `sepEndBy1` eol <* eof :: Parser [[Int]]
  let h = length rows
      w = length . head $ rows
  case grid (w, h) (concat rows) of
    Just g -> return g
    Nothing -> fail $ "Something went wrong, check input dimensions? Got " <> show (w, h)

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
