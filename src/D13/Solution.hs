module D13.Solution (solve) where

import AOC.Parser
import Control.Applicative (liftA2)
import Data.List (elemIndex, findIndices, sort)
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
solve1 = print . sum . fmap (+ 1) . findIndices (uncurry (<=))

solve2 :: Input -> IO ()
solve2 inp =
  let loc1 = singleton . singleton . Value $ 2
      loc2 = singleton . singleton . Value $ 6
      orderedPackets = sort . (loc1 :) . (loc2 :) . concatMap (\(x, y) -> [x, y]) $ inp
   in print $ liftA2 (*) (findIdx loc1 orderedPackets) (findIdx loc2 orderedPackets)
  where
    singleton :: Packet a -> Packet a
    singleton = Node . (: [])
    findIdx :: Eq a => a -> [a] -> Maybe Int
    findIdx p ps = (1 +) <$> elemIndex p ps

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
  solve2 inp
