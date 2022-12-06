module D06.Solution where

import AOC.Parser
import Data.List (find, tails)
import Text.Megaparsec

type Input = String

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x : xs) = x `notElem` xs && allUnique xs

---

solveAny :: Int -> Input -> IO ()
solveAny len = print . fmap fst . find (allUnique . take len . snd) . zip [len ..] . tails

solve1 :: Input -> IO ()
solve1 = solveAny 4

solve2 :: Input -> IO ()
solve2 = solveAny 14

---

inputP :: Parser Input
inputP = anyLineP <* eof

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
