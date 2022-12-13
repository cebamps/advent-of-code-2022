module D13.Solution (solve) where

import AOC.Parser
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type Input = [(Packet Int, Packet Int)]

-- unlabeled rose tree
data Packet a = Value a | Node [Packet a] deriving (Eq, Show)

instance Ord a => Ord (Packet a) where
  compare (Value x) (Value y) = compare x y
  -- coincides with default lexicographic ordering of lists
  compare (Node xs) (Node ys) = compare xs ys
  compare vx@(Value _) y = compare (Node [vx]) y
  compare x vy@(Value _) = compare x (Node [vy])

---

solve1 :: Input -> IO ()
solve1 =
  print
    . sum
    . fmap fst
    . filter (uncurry (<=) . snd)
    . zip [(1 :: Int) ..]

---

inputP :: Parser Input
inputP =
  let pairP = (,) <$> packetP <* eol <*> packetP <* eol
   in pairP `sepEndBy1` eol <* eof

packetP, valueP, nodeP :: Parser (Packet Int)
packetP = valueP <|> nodeP
valueP = Value . read <$> some digitChar
nodeP = Node <$> (char '[' *> (packetP `sepBy` char ',') <* char ']')

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
