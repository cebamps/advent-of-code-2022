module D05.Solution where

import AOC.Parser hiding (anyLineP)
import Control.Monad ((>=>))
import Data.Foldable (fold, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid (Endo (..))
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, newline, string)

type Index = Int

type Crate = Char

type Stacks = M.Map Index [Crate]

data Move = Move {mvQty :: Int, mvFrom :: Index, mvTo :: Index} deriving (Eq, Show)

type Input = (Stacks, [Move])

performMove :: Move -> Stacks -> Maybe Stacks
performMove (Move qty from to) = repeatM qty (pop from >=> (return . uncurry (push to)))

performMoveMulti :: Move -> Stacks -> Maybe Stacks
performMoveMulti (Move qty from to) = popN qty from >=> return . uncurry (pushN to)

popN :: Int -> Index -> Stacks -> Maybe ([Crate], Stacks)
popN qty idx stacks = case splitAt qty <$> M.lookup idx stacks of
  Just (h, t) -> Just (h, M.insert idx t stacks)
  _ -> Nothing

pushN :: Index -> [Crate] -> Stacks -> Stacks
pushN idx crates = flip M.alter idx $ \case
  Nothing -> Just crates
  Just cs -> Just $ crates ++ cs

push :: Index -> Crate -> Stacks -> Stacks
push idx crate = flip M.alter idx $ \case
  Nothing -> Just [crate]
  Just cs -> Just $ crate : cs

pop :: Index -> Stacks -> Maybe (Crate, Stacks)
pop idx stacks = case M.lookup idx stacks of
  Just (c : cs) -> Just (c, M.insert idx cs stacks)
  _ -> Nothing

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM 0 _ = return
repeatM n f = f >=> repeatM (n - 1) f

--

solveEither :: (Move -> Stacks -> Maybe Stacks) -> Input -> IO ()
solveEither perform (stacks, moves) = do
  let Just stacks' = foldl' (\s move -> s >>= perform move) (Just stacks) moves
  print . traverse listToMaybe $ M.elems stacks'

solve1 :: Input -> IO ()
solve1 = solveEither performMove

solve2 :: Input -> IO ()
solve2 = solveEither performMoveMulti

---

inputP :: Parser Input
inputP =
  (,)
    <$> (stacksP <* anyLineP <* newline)
    <*> movesP
    <* eof

stacksP :: Parser Stacks
stacksP = flip appEndo M.empty . fold <$> cratePushP `sepEndBy` eol
  where
    crateP :: Parser (Maybe Crate)
    crateP = Nothing <$ string "   " <|> Just <$> (char '[' *> anySingle <* char ']')

    cratePushP :: Parser (Endo Stacks)
    cratePushP = do
      inserts <- catMaybes . zipWith (fmap . (,)) [1 ..] <$> crateP `sepBy` char ' '
      return $ foldMap (uncurry pushCrate) inserts
    pushCrate :: Int -> Crate -> Endo Stacks
    pushCrate stack crate = Endo $ push stack crate

movesP :: Parser [Move]
movesP =
  ( Move
      <$> (string "move " *> intP)
      <*> (string " from " *> intP)
      <*> (string " to " *> intP)
  )
    `sepEndBy1` eol
  where
    intP :: Parser Int
    intP = read <$> some digitChar

anyLineP :: Parser String
anyLineP = manyTill anySingle newline

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
  solve2 inp
