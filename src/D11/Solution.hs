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
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = IM.IntMap (Monkey Item)

type Item = Int

newtype Op = Op {runOp :: Int -> Int}

instance Show Op where show _ = "<operation>"

data Test = Test
  { tDiv :: Int,
    tTrue :: Int,
    tFalse :: Int
  }
  deriving (Eq, Show)

runTest :: Test -> Item -> Int
runTest t it = case it `mod` tDiv t of
  0 -> tTrue t
  _ -> tFalse t

data Monkey a = Monkey
  { mId :: Int,
    mItems :: [a],
    mOp :: Op,
    mTest :: Test
  }
  deriving (Functor, Foldable, Traversable, Show)

throwItems :: Monkey Item -> (IM.IntMap [Item], Monkey Item)
throwItems m =
  let items = mItems m
      m' = m {mItems = []}
      processedItems = second (: []) . processItem m <$> items
   in (IM.fromListWith (flip (<>)) processedItems, m')

processItem :: Monkey Item -> Item -> (Int, Item)
processItem m it = do
  let afterInspect = runOp (mOp m) it
  let afterRelief = afterInspect `div` 3
  let target = runTest (mTest m) afterRelief
  (target, afterRelief)

pushItems :: IM.IntMap [Item] -> Input -> Input
pushItems itm inp = IM.foldlWithKey' f inp itm
  where
    f :: Input -> Int -> [Item] -> Input
    f inp' i its = IM.adjust (pushItems1 its) i inp'
    pushItems1 :: [Item] -> Monkey Item -> Monkey Item
    pushItems1 its m = m {mItems = mItems m ++ its}

monkeyTurn :: Int -> Input -> (InspectionLog, Input)
monkeyTurn i inp = do
  -- update current monkey with throwItems
  let (thrown, inp') = adjustF throwItems i inp
  -- update other monkeys
  let inp'' = pushItems thrown inp'
  (logItems i (countThrown thrown), inp'')
  where
    countThrown = sum . fmap length . IM.elems

runRound :: Input -> (InspectionLog, Input)
runRound inp = foldlM (flip monkeyTurn) inp (IM.keys inp)

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

solve1 :: Input -> IO ()
solve1 inp =
  let (l, _) = replicateM 20 runRound inp
   in print . getSum . product . take 2 . sortOn negate . IM.elems . getInspectionLog $ l

---

inputP :: Parser Input
inputP = do
  monkeys <- monkeyP `sepEndBy` eol <* eof
  return . IM.fromList . fmap (mId &&& id) $ monkeys

monkeyP :: Parser (Monkey Item)
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
    termP :: Parser (Int -> Int) -- reader monad with "old" as context
    termP =
      id <$ string "old"
        <|> const <$> intP
    operatorP :: Parser (Int -> Int -> Int)
    operatorP =
      (*) <$ char '*'
        <|> (+) <$ char '+'

intP :: Parser Int
intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
