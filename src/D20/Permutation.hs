module D20.Permutation
  ( Perm,
    mv,
    composeVF,
    composeFV,
  )
where

import Control.Monad (forM_, (<=<))
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

composeVV :: Perm s -> Perm s -> ST s (Perm s)
composeVV v1 v2 = composeFF (length v1) (read v2) (read v1)

composeFV :: (Int -> ST s Int) -> Perm s -> ST s (Perm s)
composeFV f v = composeFF (length v) f (read v)

composeFF :: Int -> (Int -> ST s Int) -> (Int -> ST s Int) -> ST s (Perm s)
composeFF n f1 f2 = generateM n (f2 <=< f1)

composeFiF :: Int -> (Int -> ST s Int) -> (Int -> ST s Int) -> ST s (Perm s)
composeFiF n f1 f2 = do
  vret <- new n
  Control.Monad.forM_ [0 .. n - 1] $ \i -> do
    j <- f1 i
    k <- f2 i
    write vret j k
  return vret

composeViV :: Perm s -> Perm s -> ST s (Perm s)
composeViV v1 v2 = composeFiF (length v1) (read v1) (read v2)

inverse :: Perm s -> ST s (Perm s)
inverse v = composeFiF (length v) (read v) return
