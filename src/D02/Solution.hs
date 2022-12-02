module D02.Solution where

import AOC.Parser
import Control.Applicative ((<|>))
import Data.Monoid (Sum (..))
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

data Play = Rock | Paper | Scissors deriving (Eq, Show, Enum, Bounded)

type Input = [(Play, Play)]

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp

solve1 :: Input -> IO ()
solve1 = print . getSum . foldMap (Sum . uncurry score)

score :: Play -> Play -> Int
score advPlay myPlay = winScore advPlay myPlay + shapeScore myPlay
  where
    shapeScore Rock = 1
    shapeScore Paper = 2
    shapeScore Scissors = 3
    winScore advPlay myPlay
      | advPlay == myPlay = 3
      | myPlay `beats` advPlay = 6
      | otherwise = 0

beats :: Play -> Play -> Bool
Paper `beats` Rock = True
Rock `beats` Scissors = True
Scissors `beats` Paper = True
_ `beats` _ = False

inputP :: Parser Input
inputP = ruleP `sepBy` eol <* eof

ruleP :: Parser (Play, Play)
ruleP = (,) <$> inP <* char ' ' <*> outP
  where
    inP = (Rock <$ char 'A') <|> (Paper <$ char 'B') <|> (Scissors <$ char 'C')
    outP = (Rock <$ char 'X') <|> (Paper <$ char 'Y') <|> (Scissors <$ char 'Z')