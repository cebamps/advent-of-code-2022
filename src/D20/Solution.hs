module D20.Solution (solve) where

import AOC.Parser
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import D20.Permutation (Perm, composeFV, composeVF, mv)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type LUT = Vector Int

type Input = LUT

-- There are duplicates, and the list has 5000 elements.
--
-- Idea: work with index permutations and a look-up table, all encoded as vectors.

getPos :: Int -> Int -> Int -> Int
getPos s n i
  | n == 0 = i
  | i == 0 = n `mod` (s - 1) -- presumably. The example does not show what happens to the first item when it moves back.
  | otherwise = 1 + ((i - 1 + n) `mod` (s - 1)) -- leave beginning undisturbed

-- | runs a step of the mix, updating the provided forward and inverse permutation
runMixStep :: LUT -> Int -> (Perm s, Perm s) -> ST s ()
runMixStep lut i (state, inv) = do
  pos <- MV.read inv i

  let s = MV.length state
      x = lut V.! i
      pos' = getPos s x pos

  MV.copy state =<< composeFV (return . mv pos pos') state
  MV.copy inv =<< composeVF inv (return . mv pos' pos)

  -- traceM $ "moving " <> show x <> " at " <> show pos <> " to " <> show pos'
  -- traceShowM =<< V.freeze =<< composeVF state (return . (lut V.!))

  return ()

-- | runs a full mix, updating the provided initial permutation
runMix :: LUT -> Perm s -> ST s ()
runMix lut state = do
  let s = MV.length state
  inv <- MV.generate s id
  forM_ [0 .. s - 1] $ \i -> runMixStep lut i (state, inv)

mix :: LUT -> LUT
mix lut = V.create $ do
  state <- MV.generate (V.length lut) id
  runMix lut state
  composeVF state (return . (lut V.!))

---

solve1 :: Input -> IO ()
solve1 inp =
  let mixed = mix inp
      Just zi = V.elemIndex 0 mixed
      readMixed i = mixed V.! ((zi + i) `mod` V.length mixed)
      elems =readMixed <$> [1000,2000,3000] 
  in print (sum elems, elems)

---

inputP :: Parser Input
inputP = do
  nums <- intP `sepEndBy1` eol <* eof
  -- might as well allocate all at once
  return $ V.fromListN (length nums) nums

intP :: Parser Int
intP = read <$> some (digitChar <|> char '-')

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
