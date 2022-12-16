module D15.Solution (solve) where

import AOC.Parser
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = [Sensor]

type Idx = (Int, Int)

data Sensor = Sensor {sPos :: Idx, sBeacon :: Idx} deriving (Eq, Show)

dist :: Idx -> Idx -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

type Range = (Int, Int)

-- stores a set of integers by tracking covered ranges
newtype IntRanges = IntRanges {getIntRanges :: Set Range} deriving (Show)

emptyRanges :: IntRanges
emptyRanges = IntRanges S.empty

insertRange :: Range -> IntRanges -> IntRanges
insertRange r@(xmin, xmax) (IntRanges set) =
  -- separate into disjoint left and right parts plus parts that overlap/touch the inserted range
  let (left, leftC) = S.spanAntitone (\(_, x) -> x <= xmin - 2) set
      (mid, right) = S.spanAntitone (\(x, _) -> x <= xmax + 2) leftC

      mid' = S.fromList . glue . S.toList . S.insert r $ mid
   in IntRanges $ left `S.union` mid' `S.union` right
  where
    -- works assuming that the ranges are ordered by first element
    glue :: [Range] -> [Range]
    glue (r1@(x1, x2) : r2@(x3, x4) : rs)
      | x3 >= x2 + 2 = r1 : glue (r2 : rs)
      | otherwise = glue $ (x1, max x2 x4) : rs
    glue rs = rs

deleteInRange :: Int -> IntRanges -> IntRanges
deleteInRange xd (IntRanges set) =
  let (left, leftC) = S.spanAntitone (\(_, x) -> x < xd) set
      (mid, right) = S.spanAntitone (\(x, _) -> x <= xd) leftC
      mid' = S.fromList . concatMap (cut xd) . S.toList $ mid
   in IntRanges $ left `S.union` mid' `S.union` right
  where
    cut :: Int -> Range -> [Range]
    cut x (x1, x2)
      | x1 == x && x == x2 = []
      | x1 == x = [(x + 1, x2)]
      | x2 == x = [(x1, x - 1)]
      | x1 < x && x < x2 = [(x1, x - 1), (x + 1, x2)]
      | otherwise = []

-- | computes the range in x coordinates of the area seen by a sensor at a
-- given y coordinate
xSeenRange :: Int -> Sensor -> Maybe Range
xSeenRange y s =
  let (sx, sy) = sPos s
      d = dist (sPos s) (sBeacon s)
      w = d - abs (sy - y)
   in if w >= 0
        then Just (sx - w, sx + w)
        else Nothing

xSeenRanges, xSeenRangesNoBeacons :: Int -> [Sensor] -> IntRanges
xSeenRangesNoBeacons y ss =
  foldr
    deleteInRange
    (xSeenRanges y ss)
    [bx | s <- ss, let (bx, by) = sBeacon s, y == by]
xSeenRanges y = foldr insertRange emptyRanges . mapMaybe (xSeenRange y)

rangeSize :: IntRanges -> Int
rangeSize = S.foldr' (\(x1, x2) s -> s + x2 - x1 + 1) 0 . getIntRanges

findNotInRange :: Range -> IntRanges -> Maybe Int
findNotInRange (xmin, xmax) (IntRanges set) =
  -- find the interval that covers our minimum or any later point
  case S.lookupMin $ S.dropWhileAntitone (\(_, x) -> x < xmin) set of
    Nothing -> Just xmin
    Just (x1, x2)
      | xmin < x1 -> Just xmin
      | x2 < xmax -> Just $ x2 + 1
      | otherwise -> Nothing

---

isTest :: Input -> Bool
isTest (s : _) = s == Sensor (2, 18) (-2, 15)
isTest _ = False

solve1 :: Input -> IO ()
solve1 inp = print . rangeSize . xSeenRangesNoBeacons y $ inp
  where
    y =
      if isTest inp
        then 10
        else 2000000

solve2 :: Input -> IO ()
solve2 inp =
  let rows = [(y, findNotInRange (0, xmax) r) | y <- [0 .. ymax], let r = xSeenRanges y inp]
      (by, bx) = head $ mapMaybe sequence rows
   in print $ 4000000 * bx + by
  where
    xmax =
      if isTest inp
        then 20
        else 4000000
    ymax = xmax

---

inputP :: Parser Input
inputP = sensorP `sepEndBy1` eol <* eof

sensorP :: Parser Sensor
sensorP =
  Sensor
    <$> ((,) <$> (string "Sensor at x=" *> intP) <*> (string ", y=" *> intP))
    <*> ((,) <$> (string ": closest beacon is at x=" *> intP) <*> (string ", y=" *> intP))

intP :: Parser Int
intP = read <$> some (digitChar <|> char '-')

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
