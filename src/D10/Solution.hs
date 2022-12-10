module D10.Solution (solve) where

import AOC.Parser
import Control.Monad.Trans.State (State, evalState, get, modify)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = [Instruction]

data Instruction = Add Int | Noop deriving (Eq, Show)

---

states :: Input -> [Int]
states is =
  let s0 = 1
      statesAfterCycles = flip evalState s0 . fmap concat . mapM eval $ is
      statesDuringCycles = s0 : statesAfterCycles
   in statesDuringCycles

eval :: Instruction -> State Int [Int]
eval Noop = (: []) <$> get
eval (Add x) = do
  s <- get
  modify (+ x)
  s' <- get
  return [s, s']

solve1 :: Input -> IO ()
solve1 inp =
  print . sum $
    [ (ss !! (pos - 1)) * pos
      | let ss = states inp,
        pos <- [20, 60, 100, 140, 180, 220] -- let's use inefficient indexing :)
    ]

---

inputP :: Parser Input
inputP = instP `sepEndBy1` eol <* eof
  where
    instP =
      Noop <$ string "noop"
        <|> Add <$> (string "addx " *> intP)
    intP = read <$> some (digitChar <|> char '-')

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
