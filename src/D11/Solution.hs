{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module D11.Solution (solve) where

import AOC.Parser
import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Bifunctor (second)
import Data.Foldable (foldlM)
import qualified Data.IntMap as IM
import Data.List (sortOn)
import Data.Monoid (Sum (..))
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = IM.IntMap (Monkey Int)

type Monkeys = IM.IntMap (Monkey Item)

type Item = Worry

newtype Op = Op {runOp :: Item -> Item}

instance Show Op where show _ = "<operation>"

data Test = Test
  { tDiv :: Int,
    tTrue :: Int,
    tFalse :: Int
  }
  deriving (Eq, Show)

runTest :: Test -> Item -> Int
runTest t it = case it `wMod` tDiv t of
  0 -> tTrue t
  _ -> tFalse t

data Monkey a = Monkey
  { mId :: Int,
    mItems :: [a],
    mOp :: Op,
    mTest :: Test
  }
  deriving (Functor, Foldable, Traversable, Show)

--- worry arithmetic

data Worry
  = -- | modular worry score
    ModularWorry
      Int
      -- ^ worry modulus
      Int
      -- ^ worry value
  | -- | exact worry score
    NumericWorry Int
  deriving (Show)

-- constructors

modularWorry :: [Int] -> Int -> Worry
modularWorry mods n =
  let m = product (S.fromList mods)
   in ModularWorry m (n `mod` m)

numericWorry :: Int -> Worry
numericWorry = NumericWorry

-- numerics

wLift :: (Int -> Int -> Int) -> Worry -> Worry -> Worry
wLift (#) (NumericWorry x) (NumericWorry y) = NumericWorry (x # y)
wLift (#) (ModularWorry xm x) (NumericWorry y) = ModularWorry xm $ (x # y) `mod` xm
wLift (#) wx@(NumericWorry _) wy@(ModularWorry _ _) = wLift (flip (#)) wy wx
wLift (#) (ModularWorry xm x) (ModularWorry ym y)
  | xm == ym = ModularWorry xm (x # y)
  | otherwise = error $ "incompatible moduli " <> show (xm, ym)

wPlus :: Worry -> Worry -> Worry
wPlus = wLift (+)

wTimes :: Worry -> Worry -> Worry
wTimes = wLift (*)

wDiv3 :: Worry -> Worry
wDiv3 (NumericWorry x) = NumericWorry $ x `div` 3
wDiv3 _ = error "cannot divide modular worry score"

-- lookup
wMod :: Worry -> Int -> Int
wMod (ModularWorry wm w) n = case wm `mod` n of
  0 -> w `mod` n
  _ -> error $ "incompatible moduli in lookup: " <> show (w, n)
wMod (NumericWorry w) n = w `mod` n

---

type ScoreFun = Monkey Item -> Item -> Item

throwItems :: ScoreFun -> Monkey Item -> (IM.IntMap [Item], Monkey Item)
throwItems scoreItem m =
  let items = mItems m
      m' = m {mItems = []}
      processedItems = second (: []) . processItem scoreItem m <$> items
   in (IM.fromListWith (flip (<>)) processedItems, m')

processItem :: ScoreFun -> Monkey Item -> Item -> (Int, Item)
processItem scoreItem m it =
  let it' = scoreItem m it
      target = runTest (mTest m) it'
   in (target, it')

-- part 1 -- unsafe with modular worry scores
scoreItem1 :: ScoreFun
scoreItem1 m it =
  let afterInspect = runOp (mOp m) it
      afterRelief = wDiv3 afterInspect
   in afterRelief

-- part 2
scoreItem2 :: ScoreFun
scoreItem2 m it =
  let afterInspect = runOp (mOp m) it
   in afterInspect

pushItems :: IM.IntMap [Item] -> Monkeys -> Monkeys
pushItems itm inp = IM.foldlWithKey' f inp itm
  where
    f :: Monkeys -> Int -> [Item] -> Monkeys
    f inp' i its = IM.adjust (pushItems1 its) i inp'
    pushItems1 :: [Item] -> Monkey Item -> Monkey Item
    pushItems1 its m = m {mItems = mItems m ++ its}

monkeyTurn :: ScoreFun -> Int -> Monkeys -> (InspectionLog, Monkeys)
monkeyTurn scoreItem i inp = do
  -- update current monkey with throwItems
  let (thrown, inp') = adjustF (throwItems scoreItem) i inp
  -- update other monkeys
  let inp'' = pushItems thrown inp'
  (logItems i (countThrown thrown), inp'')
  where
    countThrown = sum . fmap length . IM.elems

runRound :: ScoreFun -> Monkeys -> (InspectionLog, Monkeys)
runRound scoreItem inp = foldlM (flip $ monkeyTurn scoreItem) inp (IM.keys inp)

--- logging

newtype InspectionLog = InspectionLog {getInspectionLog :: IM.IntMap (Sum Int)} deriving (Show)

instance Semigroup InspectionLog where
  InspectionLog x <> InspectionLog y = InspectionLog $ IM.unionWith (<>) x y

instance Monoid InspectionLog where
  mempty = InspectionLog IM.empty

-- log item count thrown by one monkey
logItems :: Int -> Int -> InspectionLog
logItems i nthrow = InspectionLog $ IM.singleton i (Sum nthrow)

--- generic stuff

adjustF :: Functor f => (a -> f a) -> Int -> IM.IntMap a -> f (IM.IntMap a)
adjustF f k ms = case IM.lookup k ms of
  Nothing -> error $ "lookup failed on " <> show k
  Just m -> flip (IM.insert k) ms <$> f m

replicateM :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM n = foldr (>=>) return . replicate n

---

exactMonkeys :: Input -> Monkeys
exactMonkeys = (fmap . fmap) numericWorry

modularMonkeys :: Input -> Monkeys
modularMonkeys inp =
  let moduli = tDiv . mTest <$> IM.elems inp
   in (fmap . fmap) (modularWorry moduli) inp

solve1 :: Input -> IO ()
solve1 inp =
  let (l, _) = replicateM 20 (runRound scoreItem1) (exactMonkeys inp)
   in print . getSum . product . take 2 . sortOn negate . IM.elems . getInspectionLog $ l

solve2 :: Input -> IO ()
solve2 inp =
  let (l, _) = replicateM 10000 (runRound scoreItem2) (modularMonkeys inp)
   in print . getSum . product . take 2 . sortOn negate . IM.elems . getInspectionLog $ l

---

inputP :: Parser Input
inputP = do
  monkeys <- monkeyP `sepEndBy` eol <* eof
  return . IM.fromList . fmap (mId &&& id) $ monkeys

monkeyP :: Parser (Monkey Int)
monkeyP = do
  mId <- string "Monkey " *> intP <* char ':' <* eol
  mItems <- string "  Starting items: " *> itemsP <* eol
  mOp <- string "  Operation: " *> arithP <* eol
  mTest <- string "  Test: " *> testP
  return $ Monkey {..}
  where
    itemsP = intP `sepBy` string ", "
    testP :: Parser Test
    testP = do
      tDiv <- string "divisible by " *> intP <* eol
      tTrue <- string "    If true: throw to monkey " *> intP <* eol
      tFalse <- string "    If false: throw to monkey " *> intP <* eol
      return $ Test {..}

arithP :: Parser Op
arithP = do
  _ <- string "new = "
  t1 <- termP
  op <- char ' ' *> operatorP
  t2 <- char ' ' *> termP
  return . Op $ op <$> t1 <*> t2
  where
    termP :: Parser (Worry -> Worry) -- reader monad with "old" as context
    termP =
      id <$ string "old"
        <|> const . numericWorry <$> intP
    operatorP :: Parser (Worry -> Worry -> Worry)
    operatorP =
      wTimes <$ char '*'
        <|> wPlus <$ char '+'

intP :: Parser Int
intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
