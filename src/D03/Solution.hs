{-# OPTIONS_GHC -W #-}

module D02.Solution where

import AOC.Parser
import Data.Char (ord)
import Text.Megaparsec
import Text.Megaparsec.Char (eol, letterChar)
import Data.List (intersect)
import Data.Maybe (listToMaybe)
import Data.Monoid (Sum(..))

type Item = Char

type Input = [([Item], [Item])]

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp

findCommon :: [Item] -> [Item] -> Maybe Item
findCommon = fmap listToMaybe . intersect

priority :: Item -> Int
priority c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error $ "invalid item " ++ [c]

---

solve1 :: Input -> IO ()
solve1 = print . foldMap (Sum . maybe 0 priority . uncurry findCommon)

---

inputP :: Parser Input
inputP = fmap splitItems <$> sepEndBy1 (some letterChar) eol <* eof
 where
  splitItems xs = splitAt (length xs `div` 2) xs
