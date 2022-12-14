module D14.Solution (solve) where

import AOC.Parser
import Data.List (unfoldr)
import Data.Maybe (isJust)
import Data.Semigroup (Max (..), Min (..))
import qualified Data.Set as S
import Data.Tuple (swap)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = Field

newtype Idx = Idx {getIdx :: (Int, Int)} deriving (Eq, Show)

-- order on y coordinate first
instance Ord Idx where
  Idx x `compare` Idx y = swap x `compare` swap y

type Field = S.Set Idx

data Rule = Part1 | Part2 deriving (Show)

data State = State
  { sField :: Field,
    sRule :: Rule,
    sYmax :: Int
    -- TODO optimization: store the path taken by the previous unit
  }
  deriving (Show)

data Trajectory = Trajectory
  { tPath :: [Idx],
    -- | If sand settled somewhere, this is its index. Otherwise, it sank into
    -- the abyss.
    tSettled :: Maybe Idx
  }
  deriving (Eq, Show)

source :: Idx
source = Idx (500, 0)

state :: Rule -> Field -> State
state r f = let Idx (_, ymax) = S.findMax f in State f r ymax

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
  let -- possibly infinite, we can check that with a function after
      path = unfoldr
            ( \idx -> case nextIdx' s idx of
                [] -> Nothing -- settled
                idx' : _ -> Just (idx', idx')
            )
            source
   in case break (oob s) path of
        ([], _) -> Trajectory [] Nothing -- filled up
        (p, []) -> Trajectory p $ Just (last p) -- settled
        (p, _) -> Trajectory p Nothing -- fell into the abyss
  where
    nextIdx' st@State {sRule = Part1} i = unoccupied st $ nextIdx i
    nextIdx' st@State {sRule = Part2} i@(Idx (_, y))
      | y < sYmax st + 1 = unoccupied st $ nextIdx i
      | otherwise = []
    unoccupied State {sField = fld} = filter (`S.notMember` fld)

oob :: State -> Idx -> Bool
oob State {sRule = Part1, sYmax = ym} (Idx (_, y)) = ym < y
oob State {sRule = Part2} _ = False

-- debug

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
