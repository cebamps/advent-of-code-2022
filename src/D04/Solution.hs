module D04.Solution where

import AOC.Parser
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type Range = (Int, Int)

type Input = [(Range, Range)]

contains :: Range -> Range -> Bool
(xl, xr) `contains` (yl, yr) = xl <= yl && yr <= xr

overlaps :: Range -> Range -> Bool
(xl, xr) `overlaps` (yl, yr) = yl <= xr && xl <= yr

--

solve1 :: Input -> IO ()
solve1 = print . length . filter (\(x, y) -> x `contains` y || y `contains` x)

{-# ANN solve2 "HLint: ignore Use uncurry" #-}
solve2 :: Input -> IO ()
solve2 = print . length . filter (\(x, y) -> x `overlaps` y)

---

inputP :: Parser Input
inputP = pairP `sepEndBy` eol <* eof
  where
    pairP = (,) <$> rangeP <* char ',' <*> rangeP
    rangeP = (,) <$> (read <$> some digitChar) <* char '-' <*> (read <$> some digitChar)

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
