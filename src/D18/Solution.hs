module D18.Solution (solve) where

import AOC.Parser
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Monoid (Sum (getSum))
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type Input = [Cube]

type Cube = (Int, Int, Int)

countCollisions, countXCollisions, countYCollisions, countZCollisions :: [Cube] -> Sum Int
countZCollisions =
  foldMap (foldMap (uncurry countTouch) . zipPairs)
    . groupBy ((==) `on` \(x, y, _) -> (x, y))
    . sort
  where
    countTouch :: Cube -> Cube -> Sum Int
    countTouch (_, _, z1) (_, _, z2)
      | z1 + 1 == z2 = 1
      | otherwise = 0
countXCollisions = countZCollisions . fmap (\(x, y, z) -> (y, z, x))
countYCollisions = countZCollisions . fmap (\(x, y, z) -> (z, x, y))
countCollisions cs = countXCollisions cs <> countYCollisions cs <> countZCollisions cs

zipPairs :: [a] -> [(a, a)]
zipPairs xs = zip xs (tail xs)

---

solve1 :: Input -> IO ()
solve1 = do
  coll <- getSum <$> countCollisions
  cubes <- length
  return $ print (6 * cubes - 2 * coll)

---

inputP :: Parser Input
inputP = ((,,) <$> intP <* char ',' <*> intP <* char ',' <*> intP) `sepEndBy1` eol <* eof
  where
    intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
