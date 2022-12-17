module D16.Solution (solve) where

import AOC.Parser
import Algorithm.Search (dijkstra)
import Control.Arrow (Arrow (first), (&&&))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.List (foldl', groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, eol, string, upperChar)

type Input = Map Name Valve

type Name = String

data Valve = Valve {vId :: Name, vRate :: Int, vNeighbors :: [Name]} deriving (Eq, Show)

-- Picture this as a graph, where nodes are labeled as (valve id, time, opened
-- valves). The graph is acyclic: edges go from time to time + 1.
--
-- We want to start from one node and end up anywhere at time = 30.
--
-- Opening a valve makes the valve appear in the destination node's opened
-- valves.
--
-- The cost of one edge is the pressure released from the opened valves at the
-- head of the edge.
--
-- When moving from one valve to another, we will always take the shortest
-- path. Preprocessing the graph would help optimize the solution. I expect
-- this will be needed, because there are more valves with 0 rate than with
-- non-0 rate.
--
-- We can use Dijkstra's algorithm, if our cost function instead measures a
-- hypothetical pressure built up that is always nonnegative. This can be done
-- by replacing the total opened valve rate by (total rate - total opened valve
-- rate) in the cost function.

-- distances between start or non-zero rate valves
type Distances = Map Name (Map Name Int)

distances :: Input -> Distances
distances inp =
  M.fromList
    [ (from, d)
      | from <- S.toList kept,
        let d = flip M.restrictKeys kept $ distances1 inp from
    ]
  where
    keep v = vRate v /= 0 || vId v == "AA"
    kept = S.fromList . fmap vId . filter keep $ M.elems inp

distances1 :: Input -> Name -> Map Name Int
distances1 inp = distances1' next
  where
    next x = vNeighbors $ inp M.! x

-- compute distances using a breadth-first walk, including the starting point
distances1' :: Ord a => (a -> [a]) -> a -> Map a Int
distances1' next start = go 0 M.empty [start]
  where
    go _ seen [] = seen
    go n seen xs =
      let seen' = foldl' (\m x -> M.insert x n m) seen xs
          xs' = [x' | x <- xs, x' <- next x, not (x' `M.member` seen)]
       in go (n + 1) seen' xs'

data SearchState = SearchState {sT :: Int, sPos :: [Position], sOpen :: Set Name} deriving (Eq, Ord, Show)

-- Position in the graph. The distance represents how far away we are from the
-- node, in an undetermined direction.
data Position = Position
  { pId :: Name,
    pDistance :: Int
  }
  deriving (Eq, Ord, Show)

initStateForCount :: Int -> SearchState
initStateForCount n = SearchState 0 (replicate n (Position startNode 0)) S.empty

startNode :: String
startNode = "AA"

findPath :: Int -> SearchState -> Input -> Maybe (Int, [SearchState])
findPath finalTime initState inp = first toObjective <$> dijkstra next cost ((== finalTime) . sT) initState
  where
    inpD = distances inp
    -- next valve targets
    next :: SearchState -> [SearchState]
    next s =
      case [ SearchState {sPos = poss', sT = t', sOpen = open'}
             | (dt, node', idx, poss') <- nextMoves (sPos s) keep,
               let t' = sT s + dt,
               let open' = S.insert node' $ sOpen s,
               t' <= finalTime,
               sT s > 0 || idx == 0 -- break the symmetry
           ] of
        [] -> [s {sT = finalTime}] -- no time to get to another valve, stay put
        x -> x
      where
        keep node' =
          node' `S.notMember` sOpen s
            && node' /= startNode -- don't come back to start

    -- gives the possible moves along with the valve opened, time elapsed at
    -- the end of each move, and index of the mover
    nextMoves :: [Position] -> (Name -> Bool) -> [(Int, Name, Int, [Position])]
    nextMoves pos keep = prune . concatMap getCompose $ mapsEach (nextMovesOne keep) (zip [0 ..] pos)
      where
        -- for moves of different movers that take the same time, only keep one
        -- to avoid exponential branching
        prune :: [(Int, Name, Int, [Position])] -> [(Int, Name, Int, [Position])]
        prune mvs =
          [ mv
            | byTime <- groupOn dt mvs,
              mv <- filter (\x -> idx x == idx (head byTime)) byTime
          ]
        dt (x, _, _, _) = x
        idx (_, _, x, _) = x
        groupOnSorted f = groupBy ((==) `on` f)
        groupOn f = groupOnSorted f . sortOn f
    nextMovesOne ::
      (Name -> Bool) ->
      (Int, Position) ->
      Compose [] ((,,,) Int Name Int) (Position, (Int, Position) -> Position)
    nextMovesOne keep (idx, pos) =
      Compose
        [ (dt, node', idx, (pos', \(_, pother) -> pother {pDistance = pDistance pother + dt}))
          | (node', d) <- M.toList $ inpD M.! pId pos,
            let dt = d - pDistance pos + 1 -- walk to destination and open valve
                pos' = Position node' 0,
            keep node',
            dt >= 0
        ]

    -- The relieved pressure as stated in the problem. Valves are only ever
    -- opened on the time right before a destination node.
    objective :: SearchState -> SearchState -> Int
    objective s s' = (sT s' - sT s) * sum (vRate <$> M.restrictKeys inp (sOpen s))

    -- Adjust the objective to maximize into a nonnegative cost to minimize.
    cost s s' = (sT s' - sT s) * capacity - objective s s'
    capacity = sum $ vRate <$> inp

    -- revert back to objective value
    toObjective x = finalTime * capacity - x

mapsEach :: Functor f => (a -> f (b, a -> b)) -> [a] -> [f [b]]
mapsEach f = fmap go . splits
  where
    splits :: [a] -> [([a], a, [a])]
    splits [] = []
    splits (x : xs) = ([], x, xs) : fmap (\(h, m, t) -> (x : h, m, t)) (splits xs)

    go (h, m, t) = f m <&> \(m', g) -> fmap g h <> (m' : fmap g t)

---

solveAny :: (Int, Int) -> Input -> IO ()
solveAny (t, n) inp = case findPath t (initStateForCount n) inp of
  Nothing -> putStrLn "solution not found"
  Just (p, _) -> print p

solve1 :: Input -> IO ()
solve1 = solveAny (30, 1)

solve2 :: Input -> IO ()
solve2 = solveAny (26, 2)

---

inputP :: Parser Input
inputP = M.fromList . fmap (vId &&& id) <$> valveP `sepEndBy1` eol <* eof

valveP :: Parser Valve
valveP =
  Valve
    <$> (string "Valve " *> nameP)
    <*> (string " has flow rate=" *> intP)
    <*> ( string "; "
            *> (string "tunnels lead to valves " <|> string "tunnel leads to valve ")
            *> (nameP `sepBy1` string ", ")
        )
  where
    nameP = some upperChar
    intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
