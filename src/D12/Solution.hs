module D12.Solution (solve) where

import AOC.Parser
import Algorithm.Search (dijkstraAssoc)
import Control.Arrow ((&&&))
import qualified Data.Array as A
import Data.Char (ord)
import Data.Function (on)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

type Elevation = Word8

data Cell
  = Start
  | End
  | Other Elevation
  deriving (Eq, Show)

type Idx = (Int, Int)

type Field = A.Array Idx Cell

type Cost = Int

type Input = Field

charCell :: Char -> Maybe Cell
charCell 'S' = Just Start
charCell 'E' = Just End
charCell c | 'a' <= c && c <= 'z' = Just . Other . fromIntegral $ ord c - ord 'a'
charCell _ = Nothing

elevation :: Cell -> Elevation
elevation = \case
  Start -> startE
  End -> endE
  Other e -> e
  where
    startE = elevation . fromJust . charCell $ 'a'
    endE = elevation . fromJust . charCell $ 'z'

slope :: Field -> Idx -> Idx -> Int
slope fld = subtract `on` fromIntegral . elevation . (fld A.!)

adjacency :: Field -> Idx -> [(Idx, Cost)]
adjacency fld p =
  fmap (id &&& const 1)
    . filter ((<= 1) . slope fld p)
    $ neighbors fld p

neighbors :: Field -> Idx -> [Idx]
neighbors fld (x, y) = filter (A.inRange . A.bounds $ fld) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

navigate :: Field -> Idx -> Maybe (Cost, [Idx])
navigate fld startIdx =
  let endIdx = findIdxUnsafe (== End) fld
   in dijkstraAssoc
        (adjacency fld)
        (== endIdx)
        startIdx

findIdxUnsafe :: (a -> Bool) -> A.Array Idx a -> Idx
findIdxUnsafe f = fst . findUnsafe (f . snd) . A.assocs
  where
    findUnsafe prd = fromMaybe (error "findUnsafe failed") . find prd

findStartUnsafe :: A.Array Idx Cell -> Idx
findStartUnsafe = findIdxUnsafe (== Start)

---

solve1 :: Input -> IO ()
solve1 inp =
  case navigate inp (findStartUnsafe inp) of
    Nothing -> fail "could not find solution"
    Just (steps, _) -> print steps

solve2 :: Input -> IO ()
solve2 inp = do
  let starts = fmap fst . filter ((== 0) . elevation . snd) . A.assocs $ inp
      solutions = mapMaybe (navigate inp) starts
  print . minimum . fmap fst $ solutions

---

inputP :: Parser Field
inputP = do
  cells <- some cellP `sepEndBy1` eol <* eof
  let h = length cells
      w = length (head cells)
  return $
    A.array
      ((0, 0), (w - 1, h - 1))
      [((x, y), v) | (y, row) <- zip [0 ..] cells, (x, v) <- zip [0 ..] row]
  where
    cellP :: Parser Cell
    cellP =
      (choice . fmap char) ('S' : 'E' : ['a' .. 'z'])
        >>= maybe (fail "unknown character") return . charCell

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
