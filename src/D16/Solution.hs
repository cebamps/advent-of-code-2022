module D16.Solution (solve) where

import AOC.Parser
import Algorithm.Search (dijkstra)
import Control.Arrow (Arrow (first), (&&&))
import Data.List (foldl')
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
      | from <- kept,
        let d = M.filterWithKey (\to n -> n /= 0 && to `elem` kept) $ distances1 inp from
    ]
  where
    keep v = vRate v /= 0 || vId v == "AA"
    kept = fmap vId . filter keep $ M.elems inp

distances1 :: Input -> Name -> Map Name Int
distances1 inp = distances1' next
  where
    next x = vNeighbors $ inp M.! x

-- compute distances using a breadth-first walk
distances1' :: Ord a => (a -> [a]) -> a -> Map a Int
distances1' next start = go 0 M.empty [start]
  where
    go _ seen [] = seen
    go n seen xs =
      let seen' = foldl' (\m x -> M.insert x n m) seen xs
          xs' = [x' | x <- xs, x' <- next x, not (x' `M.member` seen)]
       in go (n + 1) seen' xs'

data SearchState = SearchState {sT :: Int, sPos :: Name, sOpen :: Set Name} deriving (Eq, Ord, Show)

initState :: SearchState
initState = SearchState 0 startNode S.empty

startNode :: String
startNode = "AA"

finalTime :: Int
finalTime = 30

findPath :: Input -> Maybe (Int, [SearchState])
findPath inp = first toObjective <$> dijkstra next cost ((== finalTime) . sT) initState
  where
    inpD = distances inp
    -- next valve targets
    next :: SearchState -> [SearchState]
    next s =
      case [ SearchState {sPos = pos', sT = t', sOpen = open'}
             | (pos', d) <- M.toList $ inpD M.! sPos s,
               pos' `S.notMember` sOpen s,
               pos' /= startNode, -- don't come back to start
               let t' = sT s + d + 1, -- walk to destination and open valve
               let open' = S.insert pos' $ sOpen s,
               t' <= finalTime
           ] of
        [] -> [s {sT = finalTime}] -- no time to get to another valve, stay put
        x -> x
    -- The relieved pressure as stated in the problem. Valves are only ever
    -- opened on the time right before a destination node.
    objective :: SearchState -> SearchState -> Int
    objective s s' = (sT s' - sT s) * sum (vRate <$> M.restrictKeys inp (sOpen s))

    -- Adjust the objective to maximize into a nonnegative cost to minimize.
    cost s s' = (sT s' - sT s) * capacity - objective s s'
    capacity = sum $ vRate <$> inp

    -- revert back to objective value
    toObjective x = finalTime * capacity - x

---

solve1 :: Input -> IO ()
solve1 inp = case findPath inp of
  Nothing -> putStrLn "solution not found"
  Just (p, _) -> print p

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
