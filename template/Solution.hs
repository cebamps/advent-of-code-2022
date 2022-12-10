module D00.Solution (solve) where

import AOC.Parser
import Text.Megaparsec
import Text.Megaparsec.Char (eol)

type Input = ()

---

solve1 :: Input -> IO ()
solve1 inp = return ()

solve2 :: Input -> IO ()
solve2 inp = return ()

-- $> readFile "inputs/d00-test.txt" >>= solve

---

inputP :: Parser Input
inputP = undefined <* eof

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
