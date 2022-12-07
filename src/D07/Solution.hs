module D07.Solution where

import AOC.Parser
import Control.Monad.Trans.State
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List (tails)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, punctuationChar, string)

type Input = [CommandLog]

data CommandLog
  = CdRoot
  | CdParent
  | CdSubdir String
  | Ls [LsEntry]
  deriving (Eq, Show)

data LsEntry
  = Dir String
  | File Int String
  deriving (Eq, Show)

type Path = [String]

size :: LsEntry -> Int
size (Dir _) = 0
size (File s _) = s

runCommand :: CommandLog -> State Path (Maybe (Path, [LsEntry]))
runCommand cmd = do
  modify $ newPath cmd
  case cmd of
    Ls lsLog -> get <&> (\p -> Just (p, lsLog))
    _ -> return Nothing
  where
    newPath CdRoot _ = []
    newPath CdParent (_ : t) = t
    newPath CdParent [] = [] -- hopefully we don't run into this!
    newPath (CdSubdir h') p = h' : p
    newPath (Ls _) s = s

-- assumes no duplicates
entries :: Input -> M.Map Path [LsEntry]
entries = M.fromList . entryStream
  where
    entryStream :: Input -> [(Path, [LsEntry])]
    entryStream = catMaybes . flip evalState [] . traverse runCommand

recursiveSizes :: M.Map Path [LsEntry] -> M.Map Path (Sum Int)
recursiveSizes x =
  let directSizes = fmap (foldMap (Sum . size)) x
      parentContribs = M.fromListWith (<>) [(c, s) | (p, s) <- M.toList directSizes, c <- tails p]
   in parentContribs

---

solve1 :: Input -> IO ()
solve1 inp = print $ getSum . fold . filter (<= 100_000) . M.elems . recursiveSizes . entries $ inp

---

inputP :: Parser Input
inputP = many cmdP <* eof

cmdP :: Parser CommandLog
cmdP =
  string "$ "
    *> ( (CdRoot <$ string "cd /" <* eol)
           <|> (CdParent <$ string "cd .." <* eol)
           <|> (CdSubdir <$> (string "cd " *> nameP) <* eol)
           <|> (Ls <$> (string "ls" *> eol *> lsOputputLineP `sepEndBy1` eol))
       )
  where
    lsOputputLineP :: Parser LsEntry
    lsOputputLineP =
      (Dir <$> (string "dir " *> nameP))
        <|> (File <$> intP <* char ' ' <*> nameP)

    intP :: Parser Int
    intP = read <$> many digitChar

    nameP :: Parser String
    nameP = many (letterChar <|> punctuationChar)

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
