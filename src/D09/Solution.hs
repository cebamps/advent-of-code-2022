module D09.Solution (solve) where

import AOC.Parser
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type Input = [(Direction, Int)]

data Direction = U | D | L | R deriving (Eq, Show, Read)

type Idx = (Int, Int)

unit :: Direction -> Idx
unit U = (0, -1)
unit D = (0, 1)
unit L = (-1, 0)
unit R = (1, 0)

(|+|) :: Idx -> Idx -> Idx
(x, y) |+| (x', y') = (x + x', y + y')

newLag :: Direction -> Idx -> Idx
newLag U (_, 1) = (0, 1)
newLag U (x, y) = (x, y + 1)
newLag D (_, -1) = (0, -1)
newLag D (x, y) = (x, y - 1)
newLag L (1, _) = (1, 0)
newLag L (x, y) = (x + 1, y)
newLag R (-1, _) = (-1, 0)
newLag R (x, y) = (x - 1, y)

---

allLags :: Idx -> [Direction] -> [Idx]
allLags = scanl $ flip newLag

allPos :: Idx -> [Direction] -> [Idx]
allPos = scanl $ \p d -> p |+| unit d

---

solve1 :: Input -> IO ()
solve1 inp =
  let inpSteps = concatMap (\(d, n) -> replicate n d) inp
      lags = allLags (0, 0) inpSteps
      pos = allPos (0,0) inpSteps
      tailPos = zipWith (|+|) lags pos
   in print . S.size . S.fromList $ tailPos

---

inputP :: Parser Input
inputP = ((,) <$> dirP <* char ' ' <*> intP) `sepEndBy1` eol <* eof
  where
    dirP :: Parser Direction
    dirP = read . (: []) <$> choice (char <$> "UDLR")
    intP :: Parser Int
    intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
