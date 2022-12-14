module D14.Solution (solve) where

import AOC.Parser
import Data.List (unfoldr)
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (Max (..), Min (..))
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = Field

newtype Idx = Idx {getIdx :: (Int, Int)} deriving (Eq, Show)

-- order on y coordinate first
instance Ord Idx where
  Idx x `compare` Idx y = swap x `compare` swap y

type Field = Set Idx

data Rule = Part1 | Part2 deriving (Show)

data State = State
  { sField :: Field,
    sRule :: Rule,
    sYmax :: Int,
    sPrevTrajectory :: Maybe Trajectory
  }
  deriving (Show)

data Trajectory = Trajectory
  { -- | Ordered sequence of indices, excluding the source, and including the
    -- last index in bounds.
    tPath :: Seq Idx,
    -- | If sand settled somewhere, this is its index. Otherwise, it sank into
    -- the abyss.
    tSettled :: Maybe Idx
  }
  deriving (Eq, Show)

source :: Idx
source = Idx (500, 0)

state :: Rule -> Field -> State
state r f = let Idx (_, ymax) = S.findMax f in State f r ymax Nothing

insert :: Idx -> State -> State
insert idx s = s {sField = S.insert idx $ sField s}

nextIdx :: Idx -> [Idx]
nextIdx (Idx (x, y)) = [Idx (x', y') | let y' = y + 1, x' <- [x, x - 1, x + 1]]

nextState :: State -> (Trajectory, State)
nextState s =
  let t@Trajectory {tSettled = settled} = findTrajectory s
      s' = maybe id insert settled s
   in (t, s')

findTrajectory :: State -> Trajectory
findTrajectory s =
  let -- Shortcut: get the starting point and the head of the trajectory from
      -- the previous trajectory.
      (trajHead, start) = fromMaybe (Empty, source) $ sPrevTrajectory s >>= trajectoryShortcut
      -- Split the in-bounds and (possibly infinite) out-of-bounds part of the
      -- tail of the trajectory.
      (pIn, pOut) = break (oob s) $ dfsFirst (nextIdx' s) start
      -- Reassemble the new path.
      path' = trajHead >< Seq.fromList pIn
   in -- Finally, a case analysis of the different outcomes
      case (Seq.null path', null pOut) of
        (False, True) -> Trajectory path' $ Just (last pIn) -- settled
        (True, _) -> Trajectory path' Nothing -- filled up
        (False, False) -> Trajectory path' Nothing -- fell into the abyss
  where
    nextIdx' st@State {sRule = Part1} i = unoccupied st $ nextIdx i
    nextIdx' st@State {sRule = Part2} i@(Idx (_, y))
      | y < sYmax st + 1 = unoccupied st $ nextIdx i
      | otherwise = []
    unoccupied State {sField = fld} = filter (`S.notMember` fld)

-- | extract the last valid part of the trajectory path before the particle
-- settled
trajectoryShortcut :: Trajectory -> Maybe (Seq Idx, Idx)
trajectoryShortcut trj = case tPath trj of
  h :|> t :|> _ -> Just (h, t)
  _ -> Nothing

-- | first branch of a depth-first search
dfsFirst :: (a -> [a]) -> a -> [a]
dfsFirst next =
  unfoldr
    ( \idx -> case next idx of
        [] -> Nothing -- settled
        idx' : _ -> Just (idx', idx')
    )

oob :: State -> Idx -> Bool
oob State {sRule = Part1, sYmax = ym} (Idx (_, y)) = ym < y
oob State {sRule = Part2} _ = False

---

solveAny :: Rule -> Input -> IO ()
solveAny r inp =
  let initState = state r inp
      history = takeWhile (isJust . tSettled . fst) $ iterate (nextState . snd) (nextState initState)
      extra = case r of
        Part1 -> 0
        Part2 -> 1 -- the state update does not settle sand at the source
   in print . (+ extra) . length $ history

solve1 :: Input -> IO ()
solve1 = solveAny Part1

solve2 :: Input -> IO ()
solve2 = solveAny Part2

---

inputP :: Parser Input
inputP = S.fromList <$> wallsP <* eof

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
  solve2 inp

--- debug

_draw :: State -> [String]
_draw st =
  let (Idx (xmin, ymin), Idx (xmax, ymax)) = bounds st
   in show ((xmin, ymin), (xmax, ymax))
        : "---"
        : [ [ c
              | x <- [xmin .. xmax],
                let c = if Idx (x, y) `S.member` sField st then 'x' else ' '
            ]
            | y <- [ymin .. ymax]
          ]

bounds :: State -> (Idx, Idx)
bounds st =
  let Just (Min xmin, Max xmax, Min ymin, Max ymax) =
        S.foldr' ((<>) . (\(Idx (x, y)) -> Just (Min x, Max x, Min y, Max y))) Nothing $ sField st
   in (Idx (xmin, ymin), Idx (xmax, ymax))
