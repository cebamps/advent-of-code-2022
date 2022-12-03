module D02.Solution where

import AOC.Parser
import Data.Bifunctor (Bifunctor (second))
import Data.Monoid (Sum (..))
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

data Play = Rock | Paper | Scissors deriving (Eq, Show, Enum, Bounded)

data Outcome = X | Y | Z deriving (Eq, Show, Enum, Bounded)

type Round = (Play, Outcome)

type Input = [Round]

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp

solve1 :: Input -> IO ()
solve1 = solveEither . fmap (second outcomeAsMove)
  where
    outcomeAsMove X = Rock
    outcomeAsMove Y = Paper
    outcomeAsMove Z = Scissors

solve2 :: Input -> IO ()
solve2 = solveEither . fmap roundAsPlay
  where
    roundAsPlay (advPlay, X) = (advPlay, pred' advPlay)
    roundAsPlay (advPlay, Y) = (advPlay, advPlay)
    roundAsPlay (advPlay, Z) = (advPlay, succ' advPlay)

solveEither :: [(Play, Play)] -> IO ()
solveEither = print . getSum . foldMap (Sum . uncurry scorePlay)

scorePlay :: Play -> Play -> Int
scorePlay advPlay myPlay = winScore advPlay myPlay + shapeScore myPlay
  where
    shapeScore Rock = 1
    shapeScore Paper = 2
    shapeScore Scissors = 3
    winScore advPlay myPlay
      | advPlay == myPlay = 3
      | myPlay `beats` advPlay = 6
      | otherwise = 0

beats :: Play -> Play -> Bool
beats x y = x == succ' y

succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' x | x == maxBound = minBound
        | otherwise = succ x

pred' :: (Eq a, Enum a, Bounded a) => a -> a
pred' x | x == minBound = maxBound
        | otherwise = pred x

---

inputP :: Parser Input
inputP = ruleP `sepBy` eol <* eof

ruleP :: Parser (Play, Outcome)
ruleP = (,) <$> inP <* char ' ' <*> outP
  where
    inP = (Rock <$ char 'A') <|> (Paper <$ char 'B') <|> (Scissors <$ char 'C')
    outP = (X <$ char 'X') <|> (Y <$ char 'Y') <|> (Z <$ char 'Z')
