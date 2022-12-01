module D01.Solution (solve) where

import Data.Void (Void)
import Text.Megaparsec
    ( parse, errorBundlePretty, sepEndBy1, some, Parsec )
import Text.Megaparsec.Char (digitChar, eol)
import Data.List (sort)
import Data.Ord (Down(..))

solve :: String -> IO ()
solve inpStr = do
  inp <- either (fail . errorBundlePretty) return $ parse inputP "input" inpStr

  solve1 inp
  solve2 inp

solve1 :: [[Int]] -> IO ()
solve1 = print . maximum . fmap sum

solve2 :: [[Int]] -> IO ()
solve2 = print . sum . take 3 . sortDesc . fmap sum
  where
    sortDesc :: Ord a => [a] -> [a]
    sortDesc = fmap getDown . sort . fmap Down

inputP :: Parsec Void String [[Int]]
inputP = oneP `sepEndBy1` eol
  where
    oneP :: Parsec Void String [Int]
    oneP = numberP `sepEndBy1` eol
    numberP :: Parsec Void String Int
    numberP = read <$> some digitChar