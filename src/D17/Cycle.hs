module D17.Cycle (floyd, floydOn, floydWith) where

import Control.Arrow (second, (***))
import Data.Function (on)

-- | Floyd's cycle-finding algorithm, using a custom equality function.
--
-- This is only reliable if the equality observes the exact same cyclicity as
-- the iterated value. Typically, the iteration has the form (\(a, b) -> (f a
-- b, g b)) and comparison is done on the second member of the tuple.
--
-- Returns the start index of the cycle, period of the cycle, value at start,
-- value after cycling once.
floydWith :: (a -> a -> Bool) -> (a -> a) -> a -> (Int, Int, a, a)
floydWith eq f x0 =
  let -- multiple of the period
      (_, (xnp, _)) = cycleWorker eq (f *** f . f) (x0, x0)
      -- start of the cycle
      (mu, (xmu, _)) = cycleWorker eq (f *** f) (x0, xnp)
      -- period and first loop
      (p, (_, xmup)) = cycleWorker eq (second f) (xmu, xmu)
   in (mu, p, xmu, xmup)

cycleWorker :: (a -> a -> Bool) -> ((a, a) -> (a, a)) -> (a, a) -> (Int, (a, a))
cycleWorker eq it x0 = go 1 (it x0)
  where
    converged = uncurry eq
    go n x
      | converged x = (n, x)
      | otherwise = go (n + 1) (it x)

-- | @floydOn@ by comparing a derived value. The same equality testing caveat
-- applies.
floydOn :: Eq b => (a -> b) -> (a -> a) -> a -> (Int, Int, a, a)
floydOn f = floydWith ((==) `on` f)

-- | Floyd's cycle-finding algorithm.
--
-- Returns the start index of the cycle, period of the cycle, value at start.
floyd :: Eq a => (a -> a) -> a -> (Int, Int, a)
floyd f x0 =
  let (mu, p, xmu, _) = floydWith (==) f x0
   in (mu, p, xmu)
