module D09.Solution (solve) where

import AOC.Parser
import Data.List (scanl')
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type Input = [(Direction, Int)]

data Direction = U | D | L | R deriving (Eq, Show, Read)

type Idx = (Int, Int)

unit :: Direction -> Idx
unit U = (0, -1)
unit D = (0, 1)
unit L = (-1, 0)
unit R = (1, 0)

(|+|) :: Idx -> Idx -> Idx
(x, y) |+| (x', y') = (x + x', y + y')

(|-|) :: Idx -> Idx -> Idx
(x, y) |-| (x', y') = (x - x', y - y')

-- | computes the new lag of a tail by clamping it back to its allowed range, according to the rules
lagClamp :: Idx -> Idx
lagClamp (x, y) | -1 <= x && x <= 1 && -1 <= y && y <= 1 = (x, y)
lagClamp (x, 0) = (signum x, 0)
lagClamp (0, y) = (0, signum y)
lagClamp (x, y) = (x - signum x, y - signum y)

-- | from an updated knot position, update its tail's position
follow :: Idx -> Idx -> Idx
follow hp' tp =
  let lag = tp |-| hp'
   in hp' |+| lagClamp lag

move :: Direction -> [Idx] -> [Idx]
move _ [] = []
move d (h : ts) = let h' = h |+| unit d in scanl' follow h' ts

---

solveAll :: Int -> Input -> IO ()
solveAll len inp =
  let inpSteps = concatMap (\(d, n) -> replicate n d) inp
      pos = scanl' (flip id) (replicate len (0, 0)) (move <$> inpSteps)
   in print . S.size . S.fromList . fmap last $ pos

solve1 :: Input -> IO ()
solve1 = solveAll 2

solve2 :: Input -> IO ()
solve2 = solveAll 10

---

inputP :: Parser Input
inputP = ((,) <$> dirP <* char ' ' <*> intP) `sepEndBy1` eol <* eof
  where
    dirP :: Parser Direction
    dirP = read . (: []) <$> choice (char <$> "UDLR")
    intP :: Parser Int
    intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
