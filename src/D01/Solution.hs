module D01.Solution (solve) where

import AOC.Parser
import Data.List (sort)
import Data.Ord (Down (..))
import Text.Megaparsec (sepEndBy1, some)
import Text.Megaparsec.Char (digitChar, eol)

solve :: String -> IO ()
solve inpStr = do
  inp <- parseOrFail inputP "input" inpStr

  solve1 inp
  solve2 inp

solve1 :: [[Int]] -> IO ()
solve1 = print . maximum . fmap sum

solve2 :: [[Int]] -> IO ()
solve2 = print . sum . take 3 . sortDesc . fmap sum
  where
    sortDesc :: Ord a => [a] -> [a]
    sortDesc = fmap getDown . sort . fmap Down

inputP :: Parser [[Int]]
inputP = oneP `sepEndBy1` eol
  where
    oneP :: Parser [Int]
    oneP = numberP `sepEndBy1` eol
    numberP :: Parser Int
    numberP = read <$> some digitChar
