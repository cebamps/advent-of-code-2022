module D03.Solution where

import AOC.Parser
import Data.Char (ord)
import Data.List (foldl1', intersect, unfoldr)
import Data.Maybe (listToMaybe)
import Data.Monoid (Sum (..))
import Text.Megaparsec
import Text.Megaparsec.Char (eol, letterChar)

type Item = Char

type Input = [([Item], [Item])]

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp

findCommon :: [[Item]] -> Maybe Item
findCommon = listToMaybe . foldl1' intersect

priority :: Item -> Int
priority c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error $ "invalid item " ++ [c]

partition :: Int -> [a] -> [[a]]
partition n = unfoldr $ \case
  [] -> Nothing
  xs -> Just $ splitAt n xs

---

solve1 :: Input -> IO ()
solve1 = print . foldMap (Sum . maybe 0 priority . findCommon . (\(l, r) -> [l, r]))

solve2 :: Input -> IO ()
solve2 = print . foldMap (Sum . maybe 0 priority . findCommon) . partition 3 . fmap (uncurry (<>))

---

inputP :: Parser Input
inputP = fmap splitItems <$> sepEndBy1 (some letterChar) eol <* eof
  where
    splitItems xs = splitAt (length xs `div` 2) xs
