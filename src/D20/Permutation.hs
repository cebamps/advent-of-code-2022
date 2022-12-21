module D20.Permutation
  ( Perm,
    mv,
    composeVF,
    composeFV,
  )
where

import Control.Monad ((<=<))
import Control.Monad.ST (ST)
import Data.Vector.Mutable
import Prelude hiding (length, read)

-- | A permutation, encoded such that the i-th element of the vector contains
-- the index of the i-th item after permutation. In other words, the vector is
-- identical to the permutation applied to [0..n-1]
type Perm s = STVector s Int

--  tells where an index has landed after moving the source to the destination

-- | Permutation function that moves one position to another.
mv :: Int -> Int -> Int -> Int
mv p p' i
  | p < p' && (i < p || p' < i) = i
  | p' < p && (i < p' || p < i) = i
  | i == p' = p
  | otherwise = i + signum (p' - p)

{-# ANN composeVF "HLint: ignore Eta reduce" #-}
composeVF :: Perm s -> (Int -> ST s Int) -> ST s (Perm s)
composeVF v f = composeFF (length v) (read v) f

composeFV :: (Int -> ST s Int) -> Perm s -> ST s (Perm s)
composeFV f v = composeFF (length v) f (read v)

composeFF :: Int -> (Int -> ST s Int) -> (Int -> ST s Int) -> ST s (Perm s)
composeFF n f1 f2 = generateM n (f2 <=< f1)
