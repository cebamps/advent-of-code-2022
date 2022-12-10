module D10.Solution (solve) where

import AOC.Parser
import Control.Monad.Trans.State (State, evalState, get, modify)
import Text.Megaparsec hiding (State, chunk)
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Input = [Instruction]

data Instruction = Add Int | Noop deriving (Eq, Show)

---

states :: Input -> [Int]
states is =
  let s0 = 1
      statesAfterCycles = flip evalState s0 . fmap concat . mapM eval $ is
      statesDuringCycles = s0 : statesAfterCycles
   in init statesDuringCycles

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

solve2 :: Input -> IO ()
solve2 inp =
  mapM_ putStrLn . chunk width $
    [ if vis then '#' else '.'
      | (t, s) <- [0 ..] `zip` states inp,
        let x = t `mod` width
            vis = x - s `elem` [-1 .. 1]
    ]
  where
    width = 40
    chunk n xs = case splitAt n xs of
      (h, []) -> [h]
      (h, t) -> h : chunk n t

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
  solve2 inp
