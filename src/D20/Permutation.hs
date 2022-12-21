module D20.Permutation
  ( Perm,
    mv,
    composeVF,
    composeFV,
  )
where

import Control.Monad ((<=<))
import Control.Monad.ST (ST)
import Data.Vector.Unboxed.Mutable
import Prelude hiding (length, read)

-- | A permutation, encoded such that the i-th element of the vector contains
-- the index of the i-th item after permutation. In other words, the vector is
-- identical to the permutation applied to [0..n-1]
type Perm s = STVector s Int

type Buffer s = Perm s

--  tells where an index has landed after moving the source to the destination

-- | Permutation function that moves one position to another.
mv :: Int -> Int -> Int -> Int
mv p p' i
  | p < p' && (i < p || p' < i) = i
  | p' < p && (i < p' || p < i) = i
  | i == p' = p
  | otherwise = i + signum (p' - p)

{-# ANN composeVF "HLint: ignore Eta reduce" #-}
composeVF :: Buffer s -> Perm s -> (Int -> ST s Int) -> ST s (Perm s)
composeVF buf v f = composeFF buf (length v) (read v) f

composeFV :: Buffer s -> (Int -> ST s Int) -> Perm s -> ST s (Perm s)
composeFV buf f v = composeFF buf (length v) f (read v)

composeFF :: Buffer s -> Int -> (Int -> ST s Int) -> (Int -> ST s Int) -> ST s (Perm s)
composeFF buf n f1 f2 =
  let x = iforM_ buf $ \i _ -> (unsafeWrite buf i <=< f2 <=< f1) i
   in buf <$ x
