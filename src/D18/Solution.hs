module D18.Solution (solve) where

import AOC.Parser
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

type Input = [Cube]

type Cube = Idx

type Idx = (Int, Int, Int)

countXCollisions, countYCollisions, countZCollisions :: [Cube] -> Sum Int
countZCollisions =
  foldMap (foldMap (uncurry countTouch) . zipPairs)
    . groupBy ((==) `on` \(x, y, _) -> (x, y))
    . sort
  where
    countTouch :: Cube -> Cube -> Sum Int
    countTouch (_, _, z1) (_, _, z2)
      | z1 + 1 == z2 = 1
      | otherwise = 0
countXCollisions = countZCollisions . fmap (\(x, y, z) -> (y, z, x))
countYCollisions = countZCollisions . fmap (\(x, y, z) -> (z, x, y))

countCollisions :: [Cube] -> Int
countCollisions cs = getSum $ countXCollisions cs <> countYCollisions cs <> countZCollisions cs

zipPairs :: [a] -> [(a, a)]
zipPairs xs = zip xs (tail xs)

---

type CubeMap = Set Cube

getX, getY, getZ :: Idx -> Int
getX (x, _, _) = x
getY (_, y, _) = y
getZ (_, _, z) = z

bounds :: Input -> (Idx, Idx)
bounds cubes =
  let (xmin, xmax) = minmax $ getX <$> cubes
      (ymin, ymax) = minmax $ getY <$> cubes
      (zmin, zmax) = minmax $ getZ <$> cubes
   in ((xmin, ymin, zmin), (xmax, ymax, zmax))
  where
    minmax xs = (minimum xs, maximum xs)

countOutsideFaces :: CubeMap -> Int
countOutsideFaces cubes = getSum . fst $ floodWorker next (S.empty, S.singleton (xmin - 1, ymin - 1, zmin - 1))
  where
    ((xmin, ymin, zmin), (xmax, ymax, zmax)) = bounds . S.toList $ cubes
    inbounds (x, y, z) =
      (xmin - 1 <= x && x <= xmax + 1)
        && (ymin - 1 <= y && y <= ymax + 1)
        && (zmin - 1 <= z && z <= zmax + 1)
    next :: Cube -> (Sum Int, [Cube])
    next cube =
      let ns = filter (`S.notMember` cubes) $ neighbors cube
       in ( Sum (6 - length ns),
            filter inbounds ns
          )

neighbors :: Cube -> [Cube]
neighbors (x, y, z) =
  [ (x - 1, y, z),
    (x + 1, y, z),
    (x, y - 1, z),
    (x, y + 1, z),
    (x, y, z - 1),
    (x, y, z + 1)
  ]

floodWorker ::
  Monad m =>
  -- | new neighbors
  (Cube -> m [Cube]) ->
  -- | (seen, edge)
  (CubeMap, CubeMap) ->
  -- | (seen', edge')
  m (CubeMap, CubeMap)
floodWorker getNext (seen, edge) = do
  next <- S.fromList . concat <$> mapM getNext (S.toList edge)
  let edge' = next `S.difference` seen

  if S.null edge'
    then return (seen, edge')
    else floodWorker getNext (seen `S.union` edge', edge')

---

solve1 :: Input -> IO ()
solve1 = do
  coll <- countCollisions
  cubes <- length
  return $ print (6 * cubes - 2 * coll)

solve2 :: Input -> IO ()
solve2 inp = print $ countOutsideFaces (S.fromList inp)

---

inputP :: Parser Input
inputP = ((,,) <$> intP <* char ',' <*> intP <* char ',' <*> intP) `sepEndBy1` eol <* eof
  where
    intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
