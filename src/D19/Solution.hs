{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module D19.Solution (solve) where

import AOC.Parser
import Algorithm.Search (aStar)
import Control.Arrow (first)
import Control.Monad (guard)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor (void)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..))
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = [Blueprint]

type Blueprint = ByMat RobotCost

type RobotCost = ByMat (Sum Int)

type RobotInventory = ByMat (Sum Int)

type Bag = ByMat (Sum Int)

data ByMat a = ByMat {bmOre :: a, bmCly :: a, bmObs :: a, bmGeo :: a} deriving (Eq, Ord, Functor, Foldable)

data Mat = Ore | Cly | Obs | Geo deriving (Eq, Show, Enum, Bounded)

mats :: [Mat]
mats = [minBound .. maxBound]

instance Show a => Show (ByMat a) where
  show bm = unwords [show mat <> "=" <> psh (bmGet mat bm) | mat <- mats]
    where
      psh s = "(" <> show s <> ")"

instance Semigroup a => Semigroup (ByMat a) where
  ByMat x1 x2 x3 x4 <> ByMat y1 y2 y3 y4 = ByMat (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)

instance Monoid a => Monoid (ByMat a) where
  mempty = ByMat mempty mempty mempty mempty

-- getter/setter
bmGet :: Mat -> (ByMat a -> a)
bmGet Ore = bmOre
bmGet Cly = bmCly
bmGet Obs = bmObs
bmGet Geo = bmGeo

bmSet :: Mat -> (a -> ByMat a -> ByMat a)
bmSet Ore x bm = bm {bmOre = x}
bmSet Cly x bm = bm {bmCly = x}
bmSet Obs x bm = bm {bmObs = x}
bmSet Geo x bm = bm {bmGeo = x}

-- monoid constructors
bmCon :: Monoid a => Mat -> a -> ByMat a
bmCon m x = bmSet m x mempty

data GameState = GameState
  { gTime :: Int,
    gBag :: Bag,
    gInventory :: RobotInventory,
    gWaited :: Maybe Bag,
    gBlueprint :: Blueprint
  }
  deriving (Eq, Ord, Show)

---

inTheRed :: Bag -> Bool
inTheRed = any (< 0)

-- | for a given material, give two updates: one that expends the cost and one
-- that updates the inventory
recipe :: Blueprint -> Mat -> (Bag, RobotInventory)
recipe bp m = let price = bmGet m bp in (negate <$> price, bmCon m 1)

data Command = Wait | Build Mat deriving (Eq, Show)

runCommand :: Command -> GameState -> Maybe GameState
runCommand cmd gs = do
  let bag = gBag gs
      inv = gInventory gs

  let (isWait, mmat, (bagUpd, invUpd)) =
        case cmd of
          Wait -> (True, Nothing, (mempty, mempty))
          Build mat -> (False, Just mat, recipe (gBlueprint gs) mat)

  -- build a robot (or don't)
  let (bag', inv') = (bag <> bagUpd, inv <> invUpd)
  guard $ (not . inTheRed) bag'

  -- collect resources (not using the new robot!)
  let bag'' = bag' <> inv

  -- Pruning optimizations:

  -- We waited at the previous round but could already afford the choice we are
  -- making now. We should have purchased the bot earlier.
  guard $ isWait || maybe True (\prevBag -> inTheRed (prevBag <> bagUpd)) (gWaited gs)

  -- We don't need to purchase this bot because we already earn enough of the
  -- material that we could not spend more. (Excluding geodes.)
  guard . not $
    maybe
      False
      ( \mat ->
          let maxSpending = fmap getMax . foldMap (fmap Max) $ gBlueprint gs
           in mat /= Geo && bmGet mat inv >= bmGet mat maxSpending
      )
      mmat

  return $
    GameState
      (gTime gs + 1)
      bag''
      inv'
      (if isWait then Just bag else Nothing)
      (gBlueprint gs)

nextStates :: GameState -> [GameState]
nextStates gs = mapMaybe (`runCommand` gs) (Wait : fmap Build mats)

initState :: Blueprint -> GameState
initState = GameState 0 mempty (bmCon Ore 1) Nothing

bestStrat :: Int -> Blueprint -> Maybe (Int, [GameState])
bestStrat timeLimit bp = first totalCostToObjective <$> aStar nextStates cost estimate ((>= timeLimit) . gTime) (initState bp)
  where
    -- In a perfect situation, we build a geode robot on every round, so at
    -- time t we gain (t-1) geodes.

    -- Assuming we have n geode bots at time t1, how many geodes do we produce from
    -- t1 (exclusive) to t2 (inclusive) at most? (n+1) + ... + (n + (t2-t1))
    bestGeoGainFrom n t1 t2 = ((2 * n + t2 - t1 + 1) * (t2 - t1)) `div` 2
    -- In a perfect strategy where we build geode bots on every round, number
    -- of geodes gained from t1 (exclusive) to t2 inclusive: (t1+1-1) + ... + (t2-1)
    -- Equivalent to bestGeoGainFrom 0 0 t2 - bestGeoGainFrom 0 0 t1.
    bestGeoGain t1 t2 = ((t2 - t1) * (t2 + t1 - 1)) `div` 2

    objectiveToCost t1 t2 n = bestGeoGain t1 t2 - n
    costToObjective = objectiveToCost

    -- How well on track are we to a perfect run.
    cost gs gs' = (objectiveToCost `on` gTime) gs gs' $ ((-) `on` getSum . bmGet Geo . gBag) gs' gs
    -- Strict lower bound on cost to end, i.e., upper bound on number of geodes
    -- we can hope to produce from here, adjusted to a cost.
    estimate gs =
      let t = gTime gs
       in objectiveToCost t timeLimit $ bestGeoGainFrom (getSum . bmGet Geo . gInventory $ gs) t timeLimit

    totalCostToObjective = costToObjective 0 timeLimit

---

solve1 :: Input -> IO ()
solve1 inp =
  let score = do
        byBp <- traverse (bestStrat 24) inp
        return $ sum . zipWith (*) [1 ..] . fmap fst $ byBp
   in print score

solve2 :: Input -> IO ()
solve2 inp =
  let score = do
        byBp <- traverse (bestStrat 32) $ take 3 inp
        return $ product . fmap fst $ byBp
   in print score

---

_showState :: GameState -> IO ()
_showState g =
  putStrLn . unlines $
    [ "state: t=" <> show (gTime g),
      "bag: " <> show (getSum <$> gBag g),
      "robots: " <> show (getSum <$> gInventory g)
    ]

---

inputP :: Parser Input
inputP = blueprintP `sepEndBy1` some eol <* eof

blueprintP :: Parser Blueprint
blueprintP =
  (string "Blueprint " *> some digitChar *> char ':')
    *> ( ByMat
           <$> priceLineP "ore"
           <*> priceLineP "clay"
           <*> priceLineP "obsidian"
           <*> priceLineP "geode"
       )
  where
    priceLineP :: String -> Parser RobotCost
    priceLineP s = ifsP *> string ("Each " <> s <> " robot costs ") *> pricesP <* char '.'
    priceP, pricesP :: Parser RobotCost
    pricesP = fold <$> priceP `sepBy1` string " and "
    priceP = do
      units <- Sum <$> intP
      _ <- char ' '
      con <- choice [bmCon mat <$ string name | (mat, name) <- [(Ore, "ore"), (Cly, "clay"), (Obs, "obsidian"), (Geo, "geode")]]
      return $ con units
    intP :: Parser Int
    intP = read <$> some digitChar

ifsP :: Parser ()
ifsP = void (eol *> string "  ") <|> void (char ' ')

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
