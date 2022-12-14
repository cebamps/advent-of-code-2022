module D14.Solution (solve) where

import AOC.Parser
import Data.List (unfoldr)
import Data.Maybe (isJust)
import Data.Semigroup (Max (..), Min (..))
import qualified Data.Set as S
import Data.Tuple (swap)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = State

newtype Idx = Idx {getIdx :: (Int, Int)} deriving (Eq, Show)

-- order on y coordinate first
instance Ord Idx where
  Idx x `compare` Idx y = swap x `compare` swap y

newtype State = State
  { sSet :: S.Set Idx
  -- TODO optimization: store the path taken by the previous unit
  }
  deriving (Eq, Show)

data Trajectory = Trajectory
  { tPath :: [Idx],
    -- | If sand settled somewhere, this is its index. Otherwise, it sank into
    -- the abyss.
    tSettled :: Maybe Idx
  }
  deriving (Eq, Show)

source :: Idx
source = Idx (500, 0)

nextIdx :: Idx -> [Idx]
nextIdx (Idx (x, y)) = [Idx (x', y') | let y' = y + 1, x' <- [x, x - 1, x + 1]]

nextState :: State -> (Trajectory, State)
nextState state =
  let t@Trajectory {tSettled = settled} = findTrajectory state
      state' = maybe id insert settled state
   in (t, state')
  where
    insert idx s = s {sSet = S.insert idx $ sSet s}

findTrajectory :: State -> Trajectory
findTrajectory s@State {sSet = set} =
  let -- possibly infinite, we can check that with a function after
      path =
        source
          : unfoldr
            ( \idx -> case filter (`S.notMember` set) (nextIdx idx) of
                [] -> Nothing -- settled
                idx' : _ -> Just (idx', idx')
            )
            source
   in case break (oob s) path of
        (p, []) -> Trajectory p $ Just (last p)
        (p, _) -> Trajectory p Nothing

oob :: State -> Idx -> Bool
oob s i = S.findMax (sSet s) < i

-- debug

_draw :: State -> [String]
_draw st =
  let (Idx (xmin, ymin), Idx (xmax, ymax)) = bounds st
   in show ((xmin, ymin), (xmax, ymax))
        : "---"
        : [ [ c
              | x <- [xmin .. xmax],
                let c = if Idx (x, y) `S.member` sSet st then 'x' else ' '
            ]
            | y <- [ymin .. ymax]
          ]

bounds :: State -> (Idx, Idx)
bounds st =
  let Just (Min xmin, Max xmax, Min ymin, Max ymax) =
        S.foldr' ((<>) . (\(Idx (x, y)) -> Just (Min x, Max x, Min y, Max y))) Nothing $ sSet st
   in (Idx (xmin, ymin), Idx (xmax, ymax))

---

solve1 :: Input -> IO ()
solve1 inp =
  let history = takeWhile (isJust . tSettled . fst) $ iterate (nextState . snd) (nextState inp)
   in print . length $ history

---

inputP :: Parser Input
inputP = State . S.fromList <$> wallsP <* eof

wallsP :: Parser [Idx]
wallsP = concatMap expand <$> cornersP `sepEndBy1` eol
  where
    expand :: [Idx] -> [Idx]
    expand [] = []
    expand [x] = [x]
    expand (x1 : x2 : xs) = wall x1 x2 ++ expand (x2 : xs)
    wall :: Idx -> Idx -> [Idx]
    wall (Idx (x1, y1)) (Idx (x2, y2))
      | x1 == x2 = [Idx (x1, y) | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2 = [Idx (x, y1) | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = error $ "Can't stretch a wall between " <> show ((x1, y1), (x2, y2))

cornersP :: Parser [Idx]
cornersP = idxP `sepBy1` string " -> "
  where
    idxP = Idx <$> ((,) <$> intP <* char ',' <*> intP)
    intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
