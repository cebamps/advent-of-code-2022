{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module D17.Solution (solve) where

import AOC.Parser
import Control.Monad (replicateM_, unless)
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import Data.Array (Array)
import qualified Data.Array as A
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, eol)

type Input = Array Int Jet

newtype Idx = Idx {getIdx :: (Int, Int)} deriving (Eq)

instance Show Idx where show (Idx x) = show x

instance Ord Idx where compare (Idx (x1, y1)) (Idx (x2, y2)) = compare (y1, x1) (y2, x2)

type Piece = Set Idx

type Field = Set Idx

data Jet = L | R deriving (Eq, Show)

-- aligned on the bottom left corner, to make the initial offset the same for all
pieces :: Array Int Piece
pieces =
  A.listArray (0, 4) . fmap (S.fromList . fmap Idx) $
    [ [(0, 0), (1, 0), (2, 0), (3, 0)], -- -
      [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)], -- +
      [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)], -- J
      [(0, 0), (0, 1), (0, 2), (0, 3)], -- I
      [(0, 0), (1, 0), (0, 1), (1, 1)] -- O
    ]

-- cyclic indexing
(!@) :: Array Int a -> Int -> a
a !@ i =
  let (l, r) = A.bounds a
      i' = l + (i - l) `mod` (r - l + 1)
   in a A.! i'

(|+|), (|-|) :: Idx -> Idx -> Idx
Idx (x1, y1) |+| Idx (x2, y2) = Idx (x1 + x2, y1 + y2)
Idx (x1, y1) |-| Idx (x2, y2) = Idx (x1 - x2, y1 - y2)

offsetPiece :: Idx -> Piece -> Piece
offsetPiece ofs = S.mapMonotonic (|+| ofs)

movePiece :: Idx -> Field -> Piece -> (Bool, Piece)
movePiece ofs f p =
  let p' = offsetPiece ofs p
      coll = collides f p'
   in (coll, if coll then p else p')

collides :: Field -> Piece -> Bool
collides f p =
  let placedRocks = not $ S.disjoint f p
      oob = any (\(Idx (x, y)) -> x < 0 || x > 6 || y < 0) p
   in oob || placedRocks

height :: Field -> Maybe Int
height fld = (\(Idx (_, y)) -> y + 1) <$> S.lookupMax fld

---

data GameState = GameState
  { sJetTime :: !Int,
    sPieceCount :: !Int,
    sJets :: !Input,
    sField :: !Field,
    sPiece :: !(Maybe Piece)
  }
  deriving (Show)

initState :: Input -> GameState
initState j = GameState 0 0 j S.empty Nothing

type St = State GameState

initPiece :: St Piece
initPiece = do
  h <- gets $ fromMaybe 0 . height . sField
  p' <- gets $ offsetPiece (Idx (2, h + 3)) . (pieces !@) . sPieceCount
  mp <- gets sPiece

  case mp of
    Nothing -> do
      modify' $ \s ->
        s
          { sPieceCount = sPieceCount s + 1,
            sPiece = Just p'
          }
      return p'
    Just p -> return p

-- TODO: return (Maybe Piece) indicating the final position of the piece
runMove' :: Idx -> St Bool
runMove' ofs = do
  p <- initPiece
  fld <- gets sField

  let (coll, p') = movePiece ofs fld p
  unless coll $ modify' $ \s -> s {sPiece = Just p'}

  return coll

runJet :: St Bool
runJet = do
  t <- gets sJetTime
  jet <- gets $ (!@ t) . sJets
  modify' $ \s -> s {sJetTime = t + 1}
  runMove' $ jetDir jet
  where
    jetDir L = Idx (-1, 0)
    jetDir R = Idx (1, 0)

runGravity :: St Bool
runGravity = runMove' $ Idx (0, -1)

runRound :: St Bool
runRound = runJet >> runGravity

placePiece :: St ()
placePiece = do
  loop
  modify' $ \s ->
    s
      { sPiece = Nothing,
        -- use of a partial function
        sField = fromJust (sPiece s) `S.union` sField s
      }
  where
    loop = do
      finished <- runRound
      unless finished loop

dump :: GameState -> String
dump s =
  let fld = sField s
      p = fromMaybe S.empty $ sPiece s
      (ymin, ymax) = (0, maximum [fromMaybe (-1) $ height set | set <- [fld, p]])
      (xmin, xmax) = (0, 6)
   in intercalate "\n" $
        [ [ if inF then '#' else if inP then 'o' else ' '
            | x <- [xmin .. xmax],
              let inF = S.member (Idx (x, y)) fld,
              let inP = S.member (Idx (x, y)) p
          ]
          | y <- [ymax, ymax - 1 .. ymin]
        ]
          <> [replicate 7 '-']

---

solve1 :: Input -> IO ()
solve1 inp = print . flip evalState (initState inp) $ do
  replicateM_ 2022 placePiece
  gets $ height . sField

---

debug :: GameState -> Int -> IO ()
debug s n = putStrLn . flip evalState s $ do
  replicateM_ n placePiece
  _ <- initPiece

  ss <- gets show
  d <- gets dump
  return $ unlines [ss, d]

---

inputP :: Parser Input
inputP = do
  jets <- some jetP <* eol <* eof
  return $ A.listArray (0, length jets - 1) jets
  where
    jetP = L <$ char '<' <|> R <$ char '>'

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
