module D12.Solution (solve) where

import AOC.Parser
import Algorithm.Search (dijkstraAssoc)
import Control.Arrow ((&&&))
import qualified Data.Array as A
import Data.Char (ord)
import Data.Function (on)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

type Elevation = Word8

data Cell
  = Start
  | End
  | Other Elevation
  deriving (Eq, Show)

data Direction = Forward | Backward deriving (Eq, Show)

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

adjacency :: Direction -> Field -> Idx -> [(Idx, Cost)]
adjacency d fld p =
  fmap (id &&& const 1)
    . filter ((<= 1) . slopeForDir d fld p)
    $ neighbors fld p
  where
    slopeForDir Forward = slope
    slopeForDir Backward = flip . slope

neighbors :: Field -> Idx -> [Idx]
neighbors fld (x, y) = filter (A.inRange . A.bounds $ fld) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

navigate :: Direction -> Field -> Maybe (Cost, [Idx])
navigate d fld =
  let endIdx = findIdxUnsafe (== End) fld
      startIdx = findIdxUnsafe (== Start) fld
      start = case d of
        Forward -> startIdx
        Backward -> endIdx
      stop = case d of
        Forward -> (== endIdx)
        Backward -> (== 0) . elevation . (fld A.!)
   in dijkstraAssoc
        (adjacency d fld)
        stop
        start
  where
    findIdxUnsafe f = fst . findUnsafe (f . snd) . A.assocs
    findUnsafe prd = fromMaybe (error "findUnsafe failed") . find prd

---

solveAny :: Direction -> Field -> IO ()
solveAny d inp =
  case navigate d inp of
    Nothing -> fail "could not find solution"
    Just (steps, _) -> print steps

solve1 :: Input -> IO ()
solve1 = solveAny Forward

solve2 :: Input -> IO ()
solve2 = solveAny Backward

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
